(define-module (izzy serio-constants))

;; Each triple consists of a key name, a pressed code, and possibly a released code.
(define-public serio-triples '( ;; Row 1
			       (key-esc 1 129)
			       (key-f1 59 187)
			       (key-f2 60 188)
			       (key-f3 61 189)
			       (key-f4 62 190)
			       (key-f5 63 191)
			       (key-f6 64 192)
			       (key-f7 65 193)
			       (key-f8 66 194)
			       (key-f9 67 195)
			       (key-f10 68 196)
			       (key-f11 87 215)
			       (key-f12 88 216)

			       ;; Row 2
			       (key-grave 41 169) ;; Backtick
			       (key-1 2 130)
			       (key-2 3 131)
			       (key-3 4 132)
			       (key-4 5 133)
			       (key-5 6 134)
			       (key-6 7 135)
			       (key-7 8 136)
			       (key-8 9 137)
			       (key-9 10 138)
			       (key-0 11 139)
			       (key-minus 12 140)
			       (key-equal 13 141)
			       (key-backspace 14 142)

			       ;; Row 3
			       (key-tab 15 143)
			       (key-q 16 144)
			       (key-w 17 145)
			       (key-e 18 146)
			       (key-r 19 147)
			       (key-t 20 148)
			       (key-y 21 149)
			       (key-u 22 150)
			       (key-i 23 151)
			       (key-o 24 152)
			       (key-p 25 153)
			       (key-leftbrace 26 154)
			       (key-rightbrace 27 155)
			       (key-backslash 43 171)

			       ;; Row 4
			       (key-capslock 58 186)
			       (key-a 30 158)
			       (key-s 31 159)
			       (key-d 32 160)
			       (key-f 33 161)
			       (key-g 34 162)
			       (key-h 35 163)
			       (key-j 36 164)
			       (key-k 37 165)
			       (key-l 38 166)
			       (key-semicolon 39 167)
			       (key-apostrophe 40 168)
			       (key-enter 28 156)

			       ;; Row 5
			       (key-leftshift 42 170)
			       (key-z 44 172)
			       (key-x 45 173)
			       (key-c 46 174)
			       (key-v 47 175)
			       (key-b 48 176)
			       (key-n 49 177)
			       (key-m 50 178)
			       (key-comma 51 179)
			       (key-dot 52 180)
			       (key-slash 53 181)
			       (key-rightshift 54 182)

			       ;; Row 6
			       (key-leftctrl 29 157)
			       (key-leftmeta 224091 224219) ;; win key
			       (key-leftalt 56 184)
			       (key-space 57 185)
			       (key-rightalt 224056 224184)
			       (key-rightmeta 224092 224220) ;; win key
			       (key-compose 224093 224221)
			       (key-rightctrl 224029 224157)

			       (key-sysrq 224042224055 224183224170)
			       (key-scrolllock 70 198)
			       (key-break 225029069225157197 0) ;; does not have a release code

			       (key-home 224071 224199)
			       (key-end 224079 224207)
			       (key-delete 224083 224211)
			       (key-pageup 224073 224201)
			       (key-pagedown 224081 224209)
			       (key-insert 224082 224210)

			       ;; Arrow Cluster
			       (key-left 224075 224203)
			       (key-right 224077 224205)
			       (key-up 224072 224200)
			       (key-down 224080 224208)

			       ;; Numpad

			       (key-numlock 69 197)
			       (key-kpslash 224053 224181)
			       (key-kpasterisk 55 183)
			       (key-kpminus 74 202)
			       (key-kp7 71 199)
			       (key-kp8 72 200)
			       (key-kp9 73 201)
			       (key-kpplus 78 206)
			       (key-kp4 75 203)
			       (key-kp5 76 204)
			       (key-kp6 77 205)
			       (key-kp1 79 207)
			       (key-kp2 80 208)
			       (key-kp3 81 209)
			       (key-kp0 82 210)
			       (key-kpdot 83 211)
			       (key-kpenter 224028 224156)))

(define-public ignored-serial-codes '(84 212 255 224042 224170 224054 224055 224182 224183 224070224198))

(define-public serial-codes '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 87 88 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210 211 215 216 224028 224029 224053 224056 224071 224072 224073 224075 224077 224079 224080 224081 224082 224083 224091 224092 224093 224156 224157 224181 224184 224199 224200 224201 224203 224205 224207 224208 224209 224210 224211 224219 224220 224221 224183 225029069225157197))
