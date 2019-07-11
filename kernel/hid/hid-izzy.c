/*
 *  USB HID quirks support for Linux
 *
 *  Copyright (c) 1999 Andreas Gal
 *  Copyright (c) 2000-2005 Vojtech Pavlik <vojtech@suse.cz>
 *  Copyright (c) 2005 Michael Haboustak <mike-@cinci.rr.com> for Concept2, Inc
 *  Copyright (c) 2006-2007 Jiri Kosina
 *  Copyright (c) 2008 Jiri Slaby <jirislaby@gmail.com>
 */

/*
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

/*
 * To insert this module use: 
 * 
 * rmmod -v ./hid-izzy.ko && rmmod -v hid_generic usbhid hid &&
 * modprobe -v hid && insmod ./hid-izzy.ko "hidinput=0"
 *
 * dmesg -C && modprobe hid_generic && modprobe usbhid && modprobe hid
 * && rmmod ./hid-izzy.ko && rmmod hid_generic && rmmod usbhid &&
 * rmmod hid && modprobe -v hid && insmod ./hid-izzy.ko "hidinput=0"
 *
 * 
 */

/*
 * TODO: Determine if it's possible to prevent input events from being
 * dispatched to certain clients.  That may be easier than the current
 * approach of loading a raw driver, and then creating an input event
 * source.  A downside is that you can't choose how to interpret raw
 * bytes received from the device.
 *
 * TODO: Be able to dynamically specify which devices are handled by
 * this driver.  
 *
 * TODO: Be able to dynamically configure the connect options for each
 * device handled by this driver.
 */

#define pr_fmt(fmt) KBUILD_MODNAME ": " fmt

#include <linux/device.h>
#include <linux/hid.h>
#include <linux/module.h>
#include <linux/slab.h>

#define IZZY_KEYBOARD 0x1
#define IZZY_MOUSE    0x2

/*Connection parameters*/

static unsigned int hidinput = 1;
module_param(hidinput, uint, 0644);
MODULE_PARM_DESC(hidinput, "Whether or not to create an hid input device."
"(0 = disabled [1] = enabled)");

#define IZZY_HID_CONNECT_HIDINPUT               0x0100000000000000
#define IZZY_HID_CONNECT_HIDINPUT_FORCE	        0x0200000000000000
#define IZZY_HID_CONNECT_HIDRAW 		0x0400000000000000
#define IZZY_HID_CONNECT_HIDDEV                 0x0800000000000000
#define IZZY_HID_CONNECT_HIDDEV_FORCE	        0x1000000000000000

#define IZZY_HID_DONT_CONNECT_HIDINPUT          0x0010000000000000
#define IZZY_HID_DONT_CONNECT_HIDINPUT_FORCE    0x0020000000000000
#define IZZY_HID_DONT_CONNECT_HIDRAW 		0x0040000000000000
#define IZZY_HID_DONT_CONNECT_HIDDEV            0x0080000000000000
#define IZZY_HID_DONT_CONNECT_HIDDEV_FORCE      0x0100000000000000

struct izzy_sc {
  unsigned long quirks;
  unsigned int fn_on;
  DECLARE_BITMAP(pressed_fn, KEY_CNT);
  DECLARE_BITMAP(pressed_numlock, KEY_CNT);
};

static int izzy_probe(struct hid_device *hdev,
		      const struct hid_device_id *id)
{
  unsigned long quirks = id->driver_data;
  struct izzy_sc *isc;
  unsigned int connect_mask = HID_CONNECT_DEFAULT;
  int ret;

  isc = devm_kzalloc(&hdev->dev, sizeof(*isc), GFP_KERNEL);
  if (isc == NULL) {
    hid_err(hdev, "can't alloc izzy descriptor\n");
    return -ENOMEM;
  }

  isc->quirks = quirks;

  hid_set_drvdata(hdev, isc);

  ret = hid_parse(hdev);

  if (ret) {
    hid_err(hdev, "parse failed\n");
    return ret;
  }

  if (!hidinput)
    connect_mask &= ~HID_CONNECT_HIDINPUT;

  /* if (quirks & IZZY_HID_CONNECT_HIDINPUT) */
  /*   connect_mask |= HID_CONNECT_HIDINPUT; */

  /* if (quirks & IZZY_HID_CONNECT_HIDINPUT_FORCE) */
  /*   connect_mask |= HID_CONNECT_HIDINPUT_FORCE; */

  /* if (quirks & IZZY_HID_CONNECT_HIDRAW) */
  /*   connect_mask |= HID_CONNECT_HIDRAW; */

  /* if (quirks & IZZY_HID_CONNECT_HIDDEV) */
  /*   connect_mask |= HID_CONNECT_HIDDEV; */

  /* if (quirks & IZZY_HID_CONNECT_HIDDEV_FORCE) */
  /*   connect_mask |= HID_CONNECT_HIDDEV_FORCE; */

  ret = hid_hw_start(hdev, connect_mask);
  if (ret) {
    hid_err(hdev, "hw start failed\n");
    return ret;
  }

  return 0;
}

static const struct hid_device_id izzy_devices[] = {
  //USB_VENDOR_ID_KENSINGTON
  //{ HID_USB_DEVICE(0x047d, 0x1020), .driver_data = IZZY_MOUSE },
  //USB_VENDOR_ID_CHESEN
  { HID_USB_DEVICE(0x0a81, 0x0205), .driver_data = IZZY_KEYBOARD | IZZY_MOUSE },
  { }
};

MODULE_DEVICE_TABLE(hid, izzy_devices);

static struct hid_driver izzy_driver = {
  .name = "izzy",
  .id_table = izzy_devices,
  .probe = izzy_probe,
};

module_hid_driver(izzy_driver);

MODULE_LICENSE("GPL");
