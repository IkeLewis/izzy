(define-module (izzy hid-constants)
  #:export (type-name code-name value-name)
  #:use-module (oop goops))

;;; Most of the keys/buttons are modeled after USB HUT 1.12 (see
;; http://www.usb.org/developers/hidpage).

;;; Private constants: code-names, event-names, value-names

(define code-names '(key-reserved
		     key-esc
		     key-1
		     key-2
		     key-3
		     key-4
		     key-5
		     key-6
		     key-7
		     key-8
		     key-9
		     key-0
		     key-minus
		     key-equal
		     key-backspace
		     key-tab
		     key-q
		     key-w
		     key-e
		     key-r
		     key-t
		     key-y
		     key-u
		     key-i
		     key-o
		     key-p
		     key-leftbrace
		     key-rightbrace
		     key-enter
		     key-leftctrl
		     key-a
		     key-s
		     key-d
		     key-f
		     key-g
		     key-h
		     key-j
		     key-k
		     key-l
		     key-semicolon
		     key-apostrophe
		     key-grave
		     key-leftshift
		     key-backslash
		     key-z
		     key-x
		     key-c
		     key-v
		     key-b
		     key-n
		     key-m
		     key-comma
		     key-dot
		     key-slash
		     key-rightshift
		     key-kpasterisk
		     key-leftalt
		     key-space
		     key-capslock
		     key-f1
		     key-f2
		     key-f3
		     key-f4
		     key-f5
		     key-f6
		     key-f7
		     key-f8
		     key-f9
		     key-f10
		     key-numlock
		     key-scrolllock
		     key-kp7
		     key-kp8
		     key-kp9
		     key-kpminus
		     key-kp4
		     key-kp5
		     key-kp6
		     key-kpplus
		     key-kp1
		     key-kp2
		     key-kp3
		     key-kp0
		     key-kpdot
		     key-zenkakuhankaku
		     key-102nd
		     key-f11
		     key-f12
		     key-ro
		     key-katakana
		     key-hiragana
		     key-henkan
		     key-katakanahiragana
		     key-muhenkan
		     key-kpjpcomma
		     key-kpenter
		     key-rightctrl
		     key-kpslash
		     key-sysrq
		     key-rightalt
		     key-linefeed
		     key-home
		     key-up
		     key-pageup
		     key-left
		     key-right
		     key-end
		     key-down
		     key-pagedown
		     key-insert
		     key-delete
		     key-macro
		     key-mute
		     key-volumedown
		     key-volumeup
		     key-power
		     key-kpequal
		     key-kpplusminus
		     key-pause
		     key-scale
		     key-kpcomma
		     key-hangeul
		     key-hanguel
		     key-hanja
		     key-yen
		     key-leftmeta
		     key-rightmeta
		     key-compose
		     key-stop
		     key-again
		     key-props
		     key-undo
		     key-front
		     key-copy
		     key-open
		     key-paste
		     key-find
		     key-cut
		     key-help
		     key-menu
		     key-calc
		     key-setup
		     key-sleep
		     key-wakeup
		     key-file
		     key-sendfile
		     key-deletefile
		     key-xfer
		     key-prog1
		     key-prog2
		     key-www
		     key-msdos
		     key-coffee
		     key-screenlock
		     key-direction
		     key-cyclewindows
		     key-mail
		     key-bookmarks
		     key-computer
		     key-back
		     key-forward
		     key-closecd
		     key-ejectcd
		     key-ejectclosecd
		     key-nextsong
		     key-playpause
		     key-previoussong
		     key-stopcd
		     key-record
		     key-rewind
		     key-phone
		     key-iso
		     key-config
		     key-homepage
		     key-refresh
		     key-exit
		     key-move
		     key-edit
		     key-scrollup
		     key-scrolldown
		     key-kpleftparen
		     key-kprightparen
		     key-new
		     key-redo
		     key-f13
		     key-f14
		     key-f15
		     key-f16
		     key-f17
		     key-f18
		     key-f19
		     key-f20
		     key-f21
		     key-f22
		     key-f23
		     key-f24
		     key-playcd
		     key-pausecd
		     key-prog3
		     key-prog4
		     key-dashboard
		     key-suspend
		     key-close
		     key-play
		     key-fastforward
		     key-bassboost
		     key-print
		     key-hp
		     key-camera
		     key-sound
		     key-question
		     key-email
		     key-chat
		     key-search
		     key-connect
		     key-finance
		     key-sport
		     key-shop
		     key-alterase
		     key-cancel
		     key-brightnessdown
		     key-brightnessup
		     key-media
		     key-switchvideomode
		     key-kbdillumtoggle
		     key-kbdillumdown
		     key-kbdillumup
		     key-send
		     key-reply
		     key-forwardmail
		     key-save
		     key-documents
		     key-battery
		     key-bluetooth
		     key-wlan
		     key-uwb
		     key-unknown
		     key-video-next
		     key-video-prev
		     key-brightness-cycle
		     key-brightness-auto
		     key-brightness-zero
		     key-display-off
		     key-wwan
		     key-wimax
		     key-rfkill
		     key-micmute
		     key-ok
		     key-select
		     key-goto
		     key-clear
		     key-power2
		     key-option
		     key-info
		     key-time
		     key-vendor
		     key-archive
		     key-program
		     key-channel
		     key-favorites
		     key-epg
		     key-pvr
		     key-mhp
		     key-language
		     key-title
		     key-subtitle
		     key-angle
		     key-zoom
		     key-mode
		     key-keyboard
		     key-screen
		     key-pc
		     key-tv
		     key-tv2
		     key-vcr
		     key-vcr2
		     key-sat
		     key-sat2
		     key-cd
		     key-tape
		     key-radio
		     key-tuner
		     key-player
		     key-text
		     key-dvd
		     key-aux
		     key-mp3
		     key-audio
		     key-video
		     key-directory
		     key-list
		     key-memo
		     key-calendar
		     key-red
		     key-green
		     key-yellow
		     key-blue
		     key-channelup
		     key-channeldown
		     key-first
		     key-last
		     key-ab
		     key-next
		     key-restart
		     key-slow
		     key-shuffle
		     key-break
		     key-previous
		     key-digits
		     key-teen
		     key-twen
		     key-videophone
		     key-games
		     key-zoomin
		     key-zoomout
		     key-zoomreset
		     key-wordprocessor
		     key-editor
		     key-spreadsheet
		     key-graphicseditor
		     key-presentation
		     key-database
		     key-news
		     key-voicemail
		     key-addressbook
		     key-messenger
		     key-displaytoggle
		     key-brightness-toggle
		     key-spellcheck
		     key-logoff
		     key-dollar
		     key-euro
		     key-frameback
		     key-frameforward
		     key-context-menu
		     key-media-repeat
		     key-10channelsup
		     key-10channelsdown
		     key-images
		     key-del-eol
		     key-del-eos
		     key-ins-line
		     key-del-line
		     key-fn
		     key-fn-esc
		     key-fn-f1
		     key-fn-f2
		     key-fn-f3
		     key-fn-f4
		     key-fn-f5
		     key-fn-f6
		     key-fn-f7
		     key-fn-f8
		     key-fn-f9
		     key-fn-f10
		     key-fn-f11
		     key-fn-f12
		     key-fn-1
		     key-fn-2
		     key-fn-d
		     key-fn-e
		     key-fn-f
		     key-fn-s
		     key-fn-b
		     key-brl-dot1
		     key-brl-dot2
		     key-brl-dot3
		     key-brl-dot4
		     key-brl-dot5
		     key-brl-dot6
		     key-brl-dot7
		     key-brl-dot8
		     key-brl-dot9
		     key-brl-dot10
		     key-numeric-0
		     key-numeric-1
		     key-numeric-2
		     key-numeric-3
		     key-numeric-4
		     key-numeric-5
		     key-numeric-6
		     key-numeric-7
		     key-numeric-8
		     key-numeric-9
		     key-numeric-star
		     key-numeric-pound
		     key-camera-focus
		     key-wps-button
		     key-touchpad-toggle
		     key-touchpad-on
		     key-touchpad-off
		     key-camera-zoomin
		     key-camera-zoomout
		     key-camera-up
		     key-camera-down
		     key-camera-left
		     key-camera-right
		     key-attendant-on
		     key-attendant-off
		     key-attendant-toggle
		     key-lights-toggle
		     key-als-toggle
		     key-buttonconfig
		     key-taskmanager
		     key-journal
		     key-controlpanel
		     key-appselect
		     key-screensaver
		     key-voicecommand
		     key-brightness-min
		     key-brightness-max
		     key-kbdinputassist-prev
		     key-kbdinputassist-next
		     key-kbdinputassist-prevgroup
		     key-kbdinputassist-nextgroup
		     key-kbdinputassist-accept
		     key-kbdinputassist-cancel
		     key-min-interesting
		     key-max
		     key-cnt))

(define event-names '(ev-syn 
		      ev-key
		      ev-rel
		      ev-abs
		      ev-msc
		      ev-sw
		      ev-led
		      ev-snd
		      ev-rep
		      ev-ff
		      ev-pwr
		      ev-ff-status
		      ev-max
		      ev-cnt))

(define value-names '(key-press key-release key-repeat))

;;; Public constants

(define-public key-reserved 0)
(define-public key-esc 1)
(define-public key-1 2)
(define-public key-2 3)
(define-public key-3 4)
(define-public key-4 5)
(define-public key-5 6)
(define-public key-6 7)
(define-public key-7 8)
(define-public key-8 9)
(define-public key-9 10)
(define-public key-0 11)
(define-public key-minus 12)
(define-public key-equal 13)
(define-public key-backspace 14)
(define-public key-tab 15)
(define-public key-q 16)
(define-public key-w 17)
(define-public key-e 18)
(define-public key-r 19)
(define-public key-t 20)
(define-public key-y 21)
(define-public key-u 22)
(define-public key-i 23)
(define-public key-o 24)
(define-public key-p 25)
(define-public key-leftbrace 26)
(define-public key-rightbrace 27)
(define-public key-enter 28)
(define-public key-leftctrl 29)
(define-public key-a 30)
(define-public key-s 31)
(define-public key-d 32)
(define-public key-f 33)
(define-public key-g 34)
(define-public key-h 35)
(define-public key-j 36)
(define-public key-k 37)
(define-public key-l 38)
(define-public key-semicolon 39)
(define-public key-apostrophe 40)
(define-public key-grave 41)
(define-public key-leftshift 42)
(define-public key-backslash 43)
(define-public key-z 44)
(define-public key-x 45)
(define-public key-c 46)
(define-public key-v 47)
(define-public key-b 48)
(define-public key-n 49)
(define-public key-m 50)
(define-public key-comma 51)
(define-public key-dot 52)
(define-public key-slash 53)
(define-public key-rightshift 54)
(define-public key-kpasterisk 55)
(define-public key-leftalt 56)
(define-public key-space 57)
(define-public key-capslock 58)
(define-public key-f1 59)
(define-public key-f2 60)
(define-public key-f3 61)
(define-public key-f4 62)
(define-public key-f5 63)
(define-public key-f6 64)
(define-public key-f7 65)
(define-public key-f8 66)
(define-public key-f9 67)
(define-public key-f10 68)
(define-public key-numlock 69)
(define-public key-scrolllock 70)
(define-public key-kp7 71)
(define-public key-kp8 72)
(define-public key-kp9 73)
(define-public key-kpminus 74)
(define-public key-kp4 75)
(define-public key-kp5 76)
(define-public key-kp6 77)
(define-public key-kpplus 78)
(define-public key-kp1 79)
(define-public key-kp2 80)
(define-public key-kp3 81)
(define-public key-kp0 82)
(define-public key-kpdot 83)
(define-public key-zenkakuhankaku 85)
(define-public key-102nd 86)
(define-public key-f11 87)
(define-public key-f12 88)
(define-public key-ro 89)
(define-public key-katakana 90)
(define-public key-hiragana 91)
(define-public key-henkan 92)
(define-public key-katakanahiragana 93)
(define-public key-muhenkan 94)
(define-public key-kpjpcomma 95)
(define-public key-kpenter 96)
(define-public key-rightctrl 97)
(define-public key-kpslash 98)
(define-public key-sysrq 99)
(define-public key-rightalt 100)
(define-public key-linefeed 101)
(define-public key-home 102)
(define-public key-up 103)
(define-public key-pageup 104)
(define-public key-left 105)
(define-public key-right 106)
(define-public key-end 107)
(define-public key-down 108)
(define-public key-pagedown 109)
(define-public key-insert 110)
(define-public key-delete 111)
(define-public key-macro 112)
(define-public key-mute 113)
(define-public key-volumedown 114)
(define-public key-volumeup 115)
(define-public key-power 116)
(define-public key-kpequal 117)
(define-public key-kpplusminus 118)
(define-public key-pause 119)
(define-public key-scale 120)
(define-public key-kpcomma 121)
(define-public key-hangeul 122)
(define-public key-hanguel key-hangeul)
(define-public key-hanja 123)
(define-public key-yen 124)
(define-public key-leftmeta 125)
(define-public key-rightmeta 126)
(define-public key-compose 127)
(define-public key-stop 128)
(define-public key-again 129)
(define-public key-props 130)
(define-public key-undo 131)
(define-public key-front 132)
(define-public key-copy 133)
(define-public key-open 134)
(define-public key-paste 135)
(define-public key-find 136)
(define-public key-cut 137)
(define-public key-help 138)
(define-public key-menu 139)
(define-public key-calc 140)
(define-public key-setup 141)
(define-public key-sleep 142)
(define-public key-wakeup 143)
(define-public key-file 144)
(define-public key-sendfile 145)
(define-public key-deletefile 146)
(define-public key-xfer 147)
(define-public key-prog1 148)
(define-public key-prog2 149)
(define-public key-www 150)
(define-public key-msdos 151)
(define-public key-coffee 152)
(define-public key-screenlock key-coffee)
(define-public key-direction 153)
(define-public key-cyclewindows 154)
(define-public key-mail 155)
(define-public key-bookmarks 156)
(define-public key-computer 157)
(define-public key-back 158)
(define-public key-forward 159)
(define-public key-closecd 160)
(define-public key-ejectcd 161)
(define-public key-ejectclosecd 162)
(define-public key-nextsong 163)
(define-public key-playpause 164)
(define-public key-previoussong 165)
(define-public key-stopcd 166)
(define-public key-record 167)
(define-public key-rewind 168)
(define-public key-phone 169)
(define-public key-iso 170)
(define-public key-config 171)
(define-public key-homepage 172)
(define-public key-refresh 173)
(define-public key-exit 174)
(define-public key-move 175)
(define-public key-edit 176)
(define-public key-scrollup 177)
(define-public key-scrolldown 178)
(define-public key-kpleftparen 179)
(define-public key-kprightparen 180)
(define-public key-new 181)
(define-public key-redo 182)
(define-public key-f13 183)
(define-public key-f14 184)
(define-public key-f15 185)
(define-public key-f16 186)
(define-public key-f17 187)
(define-public key-f18 188)
(define-public key-f19 189)
(define-public key-f20 190)
(define-public key-f21 191)
(define-public key-f22 192)
(define-public key-f23 193)
(define-public key-f24 194)
(define-public key-playcd 200)
(define-public key-pausecd 201)
(define-public key-prog3 202)
(define-public key-prog4 203)
(define-public key-dashboard 204)
(define-public key-suspend 205)
(define-public key-close 206)
(define-public key-play 207)
(define-public key-fastforward 208)
(define-public key-bassboost 209)
(define-public key-print 210)
(define-public key-hp 211)
(define-public key-camera 212)
(define-public key-sound 213)
(define-public key-question 214)
(define-public key-email 215)
(define-public key-chat 216)
(define-public key-search 217)
(define-public key-connect 218)
(define-public key-finance 219)
(define-public key-sport 220)
(define-public key-shop 221)
(define-public key-alterase 222)
(define-public key-cancel 223)
(define-public key-brightnessdown 224)
(define-public key-brightnessup 225)
(define-public key-media 226)
(define-public key-switchvideomode 227)
(define-public key-kbdillumtoggle 228)
(define-public key-kbdillumdown 229)
(define-public key-kbdillumup 230)
(define-public key-send 231)
(define-public key-reply 232)
(define-public key-forwardmail 233)
(define-public key-save 234)
(define-public key-documents 235)
(define-public key-battery 236)
(define-public key-bluetooth 237)
(define-public key-wlan 238)
(define-public key-uwb 239)
(define-public key-unknown 240)
(define-public key-video-next 241)
(define-public key-video-prev 242)
(define-public key-brightness-cycle 243)
(define-public key-brightness-auto 244)
(define-public key-brightness-zero key-brightness-auto)
(define-public key-display-off 245)
(define-public key-wwan 246)
(define-public key-wimax key-wwan)
(define-public key-rfkill 247)
(define-public key-micmute 248)
(define-public key-ok #x160)
(define-public key-select #x161)
(define-public key-goto #x162)
(define-public key-clear #x163)
(define-public key-power2 #x164)
(define-public key-option #x165)
(define-public key-info #x166)
(define-public key-time #x167)
(define-public key-vendor #x168)
(define-public key-archive #x169)
(define-public key-program #x16a)
(define-public key-channel #x16b)
(define-public key-favorites #x16c)
(define-public key-epg #x16d)
(define-public key-pvr #x16e)
(define-public key-mhp #x16f)
(define-public key-language #x170)
(define-public key-title #x171)
(define-public key-subtitle #x172)
(define-public key-angle #x173)
(define-public key-zoom #x174)
(define-public key-mode #x175)
(define-public key-keyboard #x176)
(define-public key-screen #x177)
(define-public key-pc #x178)
(define-public key-tv #x179)
(define-public key-tv2 #x17a)
(define-public key-vcr #x17b)
(define-public key-vcr2 #x17c)
(define-public key-sat #x17d)
(define-public key-sat2 #x17e)
(define-public key-cd #x17f)
(define-public key-tape #x180)
(define-public key-radio #x181)
(define-public key-tuner #x182)
(define-public key-player #x183)
(define-public key-text #x184)
(define-public key-dvd #x185)
(define-public key-aux #x186)
(define-public key-mp3 #x187)
(define-public key-audio #x188)
(define-public key-video #x189)
(define-public key-directory #x18a)
(define-public key-list #x18b)
(define-public key-memo #x18c)
(define-public key-calendar #x18d)
(define-public key-red #x18e)
(define-public key-green #x18f)
(define-public key-yellow #x190)
(define-public key-blue #x191)
(define-public key-channelup #x192)
(define-public key-channeldown #x193)
(define-public key-first #x194)
(define-public key-last #x195)
(define-public key-ab #x196)
(define-public key-next #x197)
(define-public key-restart #x198)
(define-public key-slow #x199)
(define-public key-shuffle #x19a)
(define-public key-break #x19b)
(define-public key-previous #x19c)
(define-public key-digits #x19d)
(define-public key-teen #x19e)
(define-public key-twen #x19f)
(define-public key-videophone #x1a0)
(define-public key-games #x1a1)
(define-public key-zoomin #x1a2)
(define-public key-zoomout #x1a3)
(define-public key-zoomreset #x1a4)
(define-public key-wordprocessor #x1a5)
(define-public key-editor #x1a6)
(define-public key-spreadsheet #x1a7)
(define-public key-graphicseditor #x1a8)
(define-public key-presentation #x1a9)
(define-public key-database #x1aa)
(define-public key-news #x1ab)
(define-public key-voicemail #x1ac)
(define-public key-addressbook #x1ad)
(define-public key-messenger #x1ae)
(define-public key-displaytoggle #x1af)
(define-public key-brightness-toggle key-displaytoggle)
(define-public key-spellcheck #x1b0)
(define-public key-logoff #x1b1)
(define-public key-dollar #x1b2)
(define-public key-euro #x1b3)
(define-public key-frameback #x1b4)
(define-public key-frameforward #x1b5)
(define-public key-context-menu #x1b6)
(define-public key-media-repeat #x1b7)
(define-public key-10channelsup #x1b8)
(define-public key-10channelsdown #x1b9)
(define-public key-images #x1ba)
(define-public key-del-eol #x1c0)
(define-public key-del-eos #x1c1)
(define-public key-ins-line #x1c2)
(define-public key-del-line #x1c3)
(define-public key-fn #x1d0)
(define-public key-fn-esc #x1d1)
(define-public key-fn-f1 #x1d2)
(define-public key-fn-f2 #x1d3)
(define-public key-fn-f3 #x1d4)
(define-public key-fn-f4 #x1d5)
(define-public key-fn-f5 #x1d6)
(define-public key-fn-f6 #x1d7)
(define-public key-fn-f7 #x1d8)
(define-public key-fn-f8 #x1d9)
(define-public key-fn-f9 #x1da)
(define-public key-fn-f10 #x1db)
(define-public key-fn-f11 #x1dc)
(define-public key-fn-f12 #x1dd)
(define-public key-fn-1 #x1de)
(define-public key-fn-2 #x1df)
(define-public key-fn-d #x1e0)
(define-public key-fn-e #x1e1)
(define-public key-fn-f #x1e2)
(define-public key-fn-s #x1e3)
(define-public key-fn-b #x1e4)
(define-public key-brl-dot1 #x1f1)
(define-public key-brl-dot2 #x1f2)
(define-public key-brl-dot3 #x1f3)
(define-public key-brl-dot4 #x1f4)
(define-public key-brl-dot5 #x1f5)
(define-public key-brl-dot6 #x1f6)
(define-public key-brl-dot7 #x1f7)
(define-public key-brl-dot8 #x1f8)
(define-public key-brl-dot9 #x1f9)
(define-public key-brl-dot10 #x1fa)
(define-public key-numeric-0 #x200)
(define-public key-numeric-1 #x201)
(define-public key-numeric-2 #x202)
(define-public key-numeric-3 #x203)
(define-public key-numeric-4 #x204)
(define-public key-numeric-5 #x205)
(define-public key-numeric-6 #x206)
(define-public key-numeric-7 #x207)
(define-public key-numeric-8 #x208)
(define-public key-numeric-9 #x209)
(define-public key-numeric-star #x20a)
(define-public key-numeric-pound #x20b)
(define-public key-camera-focus #x210)
(define-public key-wps-button #x211)
(define-public key-touchpad-toggle #x212)
(define-public key-touchpad-on #x213)
(define-public key-touchpad-off #x214)
(define-public key-camera-zoomin #x215)
(define-public key-camera-zoomout #x216)
(define-public key-camera-up #x217)
(define-public key-camera-down #x218)
(define-public key-camera-left #x219)
(define-public key-camera-right #x21a)
(define-public key-attendant-on #x21b)
(define-public key-attendant-off #x21c)
(define-public key-attendant-toggle #x21d)
(define-public key-lights-toggle #x21e)
(define-public key-als-toggle #x230)
(define-public key-buttonconfig #x240)
(define-public key-taskmanager #x241)
(define-public key-journal #x242)
(define-public key-controlpanel #x243)
(define-public key-appselect #x244)
(define-public key-screensaver #x245)
(define-public key-voicecommand #x246)
(define-public key-brightness-min #x250)
(define-public key-brightness-max #x251)
(define-public key-kbdinputassist-prev #x260)
(define-public key-kbdinputassist-next #x261)
(define-public key-kbdinputassist-prevgroup #x262)
(define-public key-kbdinputassist-nextgroup #x263)
(define-public key-kbdinputassist-accept #x264)
(define-public key-kbdinputassist-cancel #x265)
(define-public key-min-interesting key-mute)
(define-public key-max #x2ff)
(define-public key-cnt (+ key-max 1))

(define-public ev-syn #x00)
(define-public ev-key #x01)
(define-public ev-rel #x02)
(define-public ev-abs #x03)
(define-public ev-msc #x04)
(define-public ev-sw #x05)
(define-public ev-led #x11)
(define-public ev-snd #x12)
(define-public ev-rep #x14)
(define-public ev-ff #x15)
(define-public ev-pwr #x16)
(define-public ev-ff-status #x17)
(define-public ev-max #x1f)
(define-public ev-cnt (+ ev-max 1))

(define-public key-press 1)
(define-public key-release 0)
(define-public key-repeat 2)

;;; Private Methods

;; Provide an efficient way to find a name for the type of input
;; event given its integer value
(define int->type-name-ht (make-hash-table (length event-names)))

(for-each (lambda (name)
	    (hash-set! int->type-name-ht (primitive-eval name) name))
	  event-names)

;; Provide an efficient way to find a name for the type of input
;; event given its integer value
(define int->code-name-ht (make-hash-table (length code-names)))

(for-each (lambda (name)
	    (hash-set! int->code-name-ht (primitive-eval name) name))
	  code-names)

(define int->value-name-ht (make-hash-table (length value-names)))

(for-each (lambda (name)
	    (hash-set! int->value-name-ht (primitive-eval name) name))
	  value-names)

;;; Public Convenience Methods

(define-method (type-name (int <integer>))
  (hash-ref int->type-name-ht int))

(define-method (code-name (int <integer>))
  (hash-ref int->code-name-ht int))

(define-method (value-name (int <integer>))
  (hash-ref int->value-name-ht int))
