"
This pool contains all the platform independent key symbols. They are the same used by SDL2 to avoid having to perform any kind of translation.

The symbolic constants and their mapping to named KeyboardKey	for Morphic consumption are defined in the data method. Some SDL2 constants are omitted because they are rare and they do not have a corresponding mapping in KeyboardKey. One requirement in the initialize method is that all of the named constan have a corresponding mapping in KeyboardKey.
"
Class {
	#name : #OSKeySymbols,
	#superclass : #SharedPool,
	#classVars : [
		'OSK_0',
		'OSK_1',
		'OSK_2',
		'OSK_3',
		'OSK_4',
		'OSK_5',
		'OSK_6',
		'OSK_7',
		'OSK_8',
		'OSK_9',
		'OSK_AGAIN',
		'OSK_AMPERSAND',
		'OSK_ASTERISK',
		'OSK_AT',
		'OSK_BACKQUOTE',
		'OSK_BACKSLASH',
		'OSK_BACKSPACE',
		'OSK_CANCEL',
		'OSK_CAPSLOCK',
		'OSK_CARET',
		'OSK_CLEAR',
		'OSK_COLON',
		'OSK_COMMA',
		'OSK_COPY',
		'OSK_CUT',
		'OSK_DELETE',
		'OSK_DOLLAR',
		'OSK_DOWN',
		'OSK_END',
		'OSK_EQUALS',
		'OSK_ESCAPE',
		'OSK_EXCLAIM',
		'OSK_EXECUTE',
		'OSK_F1',
		'OSK_F10',
		'OSK_F11',
		'OSK_F12',
		'OSK_F13',
		'OSK_F14',
		'OSK_F15',
		'OSK_F16',
		'OSK_F17',
		'OSK_F18',
		'OSK_F19',
		'OSK_F2',
		'OSK_F20',
		'OSK_F21',
		'OSK_F22',
		'OSK_F23',
		'OSK_F24',
		'OSK_F3',
		'OSK_F4',
		'OSK_F5',
		'OSK_F6',
		'OSK_F7',
		'OSK_F8',
		'OSK_F9',
		'OSK_FIND',
		'OSK_GREATER',
		'OSK_HASH',
		'OSK_HELP',
		'OSK_HOME',
		'OSK_INSERT',
		'OSK_KP_0',
		'OSK_KP_1',
		'OSK_KP_2',
		'OSK_KP_3',
		'OSK_KP_4',
		'OSK_KP_5',
		'OSK_KP_6',
		'OSK_KP_7',
		'OSK_KP_8',
		'OSK_KP_9',
		'OSK_KP_DIVIDE',
		'OSK_KP_ENTER',
		'OSK_KP_EQUALS',
		'OSK_KP_MINUS',
		'OSK_KP_MULTIPLY',
		'OSK_KP_PERIOD',
		'OSK_KP_PLUS',
		'OSK_LALT',
		'OSK_LCTRL',
		'OSK_LEFT',
		'OSK_LEFTBRACKET',
		'OSK_LEFTPAREN',
		'OSK_LESS',
		'OSK_LGUI',
		'OSK_LSHIFT',
		'OSK_MENU',
		'OSK_MINUS',
		'OSK_NUMLOCKCLEAR',
		'OSK_PAGEDOWN',
		'OSK_PAGEUP',
		'OSK_PASTE',
		'OSK_PAUSE',
		'OSK_PERCENT',
		'OSK_PERIOD',
		'OSK_PLUS',
		'OSK_PRINTSCREEN',
		'OSK_PRIOR',
		'OSK_QUESTION',
		'OSK_QUOTE',
		'OSK_QUOTEDBL',
		'OSK_RALT',
		'OSK_RCTRL',
		'OSK_RETURN',
		'OSK_RGUI',
		'OSK_RIGHT',
		'OSK_RIGHTBRACKET',
		'OSK_RIGHTPAREN',
		'OSK_RSHIFT',
		'OSK_SCROLLLOCK',
		'OSK_SELECT',
		'OSK_SEMICOLON',
		'OSK_SLASH',
		'OSK_SPACE',
		'OSK_STOP',
		'OSK_SYSREQ',
		'OSK_TAB',
		'OSK_UNDERSCORE',
		'OSK_UNDO',
		'OSK_UNKNOWN',
		'OSK_UP',
		'OSK_a',
		'OSK_b',
		'OSK_c',
		'OSK_d',
		'OSK_e',
		'OSK_f',
		'OSK_g',
		'OSK_h',
		'OSK_i',
		'OSK_j',
		'OSK_k',
		'OSK_l',
		'OSK_m',
		'OSK_n',
		'OSK_o',
		'OSK_p',
		'OSK_q',
		'OSK_r',
		'OSK_s',
		'OSK_t',
		'OSK_u',
		'OSK_v',
		'OSK_w',
		'OSK_x',
		'OSK_y',
		'OSK_z',
		'OSKeySymbolNameToValueMap',
		'OSKeySymbolValueToKeyboardKeyMap'
	],
	#category : #'OSWindow-Core-Events'
}

{ #category : #private }
OSKeySymbols class >> data [
	^ #(
		(16r0000 OSK_UNKNOWN null)
		(16r000D OSK_RETURN return)
		(16r001B OSK_ESCAPE escape)
		(16r0008 OSK_BACKSPACE backspace)
		(16r0009 OSK_TAB tab)
		(16r0020 OSK_SPACE space)
		(16r0021 OSK_EXCLAIM exclam)
		(16r0022 OSK_QUOTEDBL quotedbl)
		(16r0023 OSK_HASH numbersign)
		(16r0025 OSK_PERCENT percent)
		(16r0024 OSK_DOLLAR underscore)
		(16r0026 OSK_AMPERSAND ampersand)
		(16r0027 OSK_QUOTE apostrophe)
		(16r0028 OSK_LEFTPAREN parenleft)
		(16r0029 OSK_RIGHTPAREN parenright)
		(16r002A OSK_ASTERISK asterisk)
		(16r002B OSK_PLUS plus)
		(16r002C OSK_COMMA comma)	
		(16r002D OSK_MINUS minus)
		(16r002E OSK_PERIOD period)
		(16r002F OSK_SLASH slash)
		(16r0030 OSK_0 0)	
		(16r0031 OSK_1 1)
		(16r0032 OSK_2 2)
		(16r0033 OSK_3 3)
		(16r0034 OSK_4 4)
		(16r0035 OSK_5 5)
		(16r0036 OSK_6 6)
		(16r0037 OSK_7 7)
		(16r0038 OSK_8 8)
		(16r0039 OSK_9 9)
		(16r003A OSK_COLON colon)
		(16r003B OSK_SEMICOLON semicolon)
		(16r003C OSK_LESS less)
		(16r003D OSK_EQUALS equal)
		(16r003E OSK_GREATER greater)
		(16r003F OSK_QUESTION question)
		(16r0040 OSK_AT at)		
		(16r005B OSK_LEFTBRACKET bracketleft)
		(16r005C OSK_BACKSLASH backslash)
		(16r005D OSK_RIGHTBRACKET bracketright)
		(16r005E OSK_CARET asciicircum)
		(16r005F OSK_UNDERSCORE underscore)
		(16r0060 OSK_BACKQUOTE grave)
		(16r0061 OSK_a a)
		(16r0062 OSK_b b)
		(16r0063 OSK_c c)
		(16r0064 OSK_d d)
		(16r0065 OSK_e e)
		(16r0066 OSK_f f)
		(16r0067 OSK_g g)
		(16r0068 OSK_h h)
		(16r0069 OSK_i i)
		(16r006A OSK_j j)
		(16r006B OSK_k k)
		(16r006C OSK_l l)
		(16r006D OSK_m m)
		(16r006E OSK_n n)
		(16r006F OSK_o o)
		(16r0070 OSK_p p)
		(16r0071 OSK_q q)
		(16r0072 OSK_r r)
		(16r0073 OSK_s s)
		(16r0074 OSK_t t)
		(16r0075 OSK_u u)
		(16r0076 OSK_v v)
		(16r0077 OSK_w w)
		(16r0078 OSK_x x)
		(16r0079 OSK_y y)
		(16r007A OSK_z z)
		(16r40000039 OSK_CAPSLOCK Caps_Lock)
		(16r4000003A OSK_F1 f1)
		(16r4000003B OSK_F2 f2)
		(16r4000003C OSK_F3 f3)
		(16r4000003D OSK_F4 f4)
		(16r4000003E OSK_F5 f5)
		(16r4000003F OSK_F6 f6)
		(16r40000040 OSK_F7 f7)
		(16r40000041 OSK_F8 f8)
		(16r40000042 OSK_F9 f9)
		(16r40000043 OSK_F10 f10)
		(16r40000044 OSK_F11 f11)
		(16r40000045 OSK_F12 f12)
		(16r40000046 OSK_PRINTSCREEN printScreen)
		(16r40000047 OSK_SCROLLLOCK scroll_Lock)
		(16r40000048 OSK_PAUSE pause)
		(16r40000049 OSK_INSERT insert)
		(16r4000004A OSK_HOME HOME)
		(16r4000004B OSK_PAGEUP Page_Up)
		(16r007F OSK_DELETE delete)
		(16r4000004D OSK_END end)
		(16r4000004E OSK_PAGEDOWN Page_Down)
		(16r4000004F OSK_RIGHT right)
		(16r40000050 OSK_LEFT left)
		(16r40000051 OSK_DOWN down)
		(16r40000052 OSK_UP up)
		(16r40000053 OSK_NUMLOCKCLEAR num_lock)
		(16r40000054 OSK_KP_DIVIDE kp_divide)
		(16r40000055 OSK_KP_MULTIPLY kp_multiply)
		(16r40000056 OSK_KP_MINUS KP_Subtract)
		(16r40000057 OSK_KP_PLUS KP_Add)
		(16r40000058 OSK_KP_ENTER kp_enter)
		(16r40000059 OSK_KP_1 kp_1)
		(16r4000005A OSK_KP_2 kp_2)
		(16r4000005B OSK_KP_3 kp_3)
		(16r4000005C OSK_KP_4 kp_4)
		(16r4000005D OSK_KP_5 kp_5)
		(16r4000005E OSK_KP_6 kp_6)
		(16r4000005F OSK_KP_7 kp_7)
		(16r40000060 OSK_KP_8 kp_8)
		(16r40000061 OSK_KP_9 kp_9)
		(16r40000062 OSK_KP_0 kp_0)
		(16r40000063 OSK_KP_PERIOD KP_Decimal)
		"(16r40000065 OSK_APPLICATION application)
		(16r40000066 OSK_POWER power)"
		(16r40000067 OSK_KP_EQUALS kp_equal)
		(16r40000068 OSK_F13 f13)
		(16r40000069 OSK_F14 f14)
		(16r4000006A OSK_F15 f15)
		(16r4000006B OSK_F16 f16)
		(16r4000006C OSK_F17 f17)
		(16r4000006D OSK_F18 f18)
		(16r4000006E OSK_F19 f19)
		(16r4000006F OSK_F20 f20)
		(16r40000070 OSK_F21 f21)
		(16r40000071 OSK_F22 f22)
		(16r40000072 OSK_F23 f23)
		(16r40000073 OSK_F24 f24)
		(16r40000074 OSK_EXECUTE execute)
		(16r40000075 OSK_HELP help)
		(16r40000076 OSK_MENU menu)
		(16r40000077 OSK_SELECT select)
		(16r40000078 OSK_STOP break	)
		(16r40000079 OSK_AGAIN redo)
		(16r4000007A OSK_UNDO undo)
		(16r4000007B OSK_CUT cut)
		(16r4000007C OSK_COPY copy)
		(16r4000007D OSK_PASTE paste)
		(16r4000007E OSK_FIND find)
		"(16r4000007F OSK_MUTE mute)
		(16r40000080 OSK_VOLUMEUP volumeup)
		(16r40000081 OSK_VOLUMEDOWN volumedown)
		(16r40000085 OSK_KP_COMMA kp_comma)
		(16r40000086 OSK_KP_EQUALSAS400 kp_equalsas400)
		(16r40000099 OSK_ALTERASE altErase)"
		(16r4000009A OSK_SYSREQ Sys_Req)
		(16r4000009B OSK_CANCEL cancel)
		(16r4000009C OSK_CLEAR clear)
		(16r4000009D OSK_PRIOR prior)
		"(16r4000009E OSK_RETURN2 return2)
		(16r4000009F OSK_SEPARATOR separator)
		(16r400000A0 OSK_OUT out)
		(16r400000A1 OSK_OPER oper)
		(16r400000A2 OSK_CLEARAGAIN clearagain)
		(16r400000A3 OSK_CRSEL CursorSelect)
		(16r400000A4 OSK_EXSEL ExSelect)
		(16r400000B0 OSK_KP_00 kp_00)
		(16r400000B1 OSK_KP_000 kp_000)
		(16r400000B2 OSK_THOUSANDSSEPARATOR thousandsseparator)
		(16r400000B3 OSK_DECIMALSEPARATOR decimalseparator)
		(16r400000B4 OSK_CURRENCYUNIT currencyunit)
		(16r400000B5 OSK_CURRENCYSUBUNIT currencysubunit)
		(16r400000B6 OSK_KP_LEFTPAREN kp_leftparen)
		(16r400000B7 OSK_KP_RIGHTPAREN kp_rightparen)
		(16r400000B8 OSK_KP_LEFTBRACE kp_leftbrace)
		(16r400000B9 OSK_KP_RIGHTBRACE kp_rightbrace)
		(16r400000BA OSK_KP_TAB kp_tab)
		(16r400000BB OSK_KP_BACKSPACE kp_backspace)
		(16r400000BC OSK_KP_A kp_a)
		(16r400000BD OSK_KP_B kp_b)
		(16r400000BE OSK_KP_C kp_c)
		(16r400000BF OSK_KP_D kp_d)
		(16r400000C0 OSK_KP_E kp_e)
		(16r400000C1 OSK_KP_F kp_f)
		(16r400000C2 OSK_KP_XOR kp_xor)
		(16r400000C3 OSK_KP_POWER kp_power)
		(16r400000C4 OSK_KP_PERCENT kp_percent)
		(16r400000C5 OSK_KP_LESS kp_less)
		(16r400000C6 OSK_KP_GREATER kp_greater)
		(16r400000C7 OSK_KP_AMPERSAND kp_ampersand)
		(16r400000C8 OSK_KP_DBLAMPERSAND kp_dblampersand)
		(16r400000C9 OSK_KP_VERTICALBAR kp_verticalbar)
		(16r400000CA OSK_KP_DBLVERTICALBAR kp_dblverticalbar)
		(16r400000CB OSK_KP_COLON kp_colon)
		(16r400000CC OSK_KP_HASH kp_hash)
		(16r400000CD OSK_KP_SPACE kp_space)
		(16r400000CE OSK_KP_AT kp_at)
		(16r400000CF OSK_KP_EXCLAM kp_exclam)
		(16r400000D0 OSK_KP_MEMSTORE kp_memstore)
		(16r400000D1 OSK_KP_MEMRECALL kp_memrecall)
		(16r400000D2 OSK_KP_MEMCLEAR kp_memclear)
		(16r400000D3 OSK_KP_MEMADD kp_memadd)
		(16r400000D4 OSK_KP_MEMSUBTRACT kp_memsubtract)
		(16r400000D5 OSK_KP_MEMMULTIPLY kp_memmultiply)
		(16r400000D6 OSK_KP_MEMDIVIDE kp_memdivide)
		(16r400000D7 OSK_KP_PLUSMINUS kp_plusminus)
		(16r400000D8 OSK_KP_CLEAR kp_clear)
		(16r400000D9 OSK_KP_CLEARENTRY kp_clearentry)
		(16r400000DA OSK_KP_BINARY kp_binary)
		(16r400000DB OSK_KP_OCTAL kp_octal)
		(16r400000DC OSK_KP_DECIMAL kp_decimal)
		(16r400000DD OSK_KP_HEXADECIMAL kp_hexadecimal)"
		(16r400000E0 OSK_LCTRL Control_L)
		(16r400000E1 OSK_LSHIFT Shift_L)
		(16r400000E2 OSK_LALT Alt_L)
		(16r400000E3 OSK_LGUI Super_L)
		(16r400000E4 OSK_RCTRL Control_R)
		(16r400000E5 OSK_RSHIFT Shift_R)
		(16r400000E6 OSK_RALT Alt_R)
		(16r400000E7 OSK_RGUI Super_R)
		"(16r40000101 OSK_MODE mode)
		(16r40000102 OSK_AUDIONEXT audioNext)
		(16r40000103 OSK_AUDIOPREV audioPrev)
		(16r40000104 OSK_AUDIOSTOP audioStop)
		(16r40000105 OSK_AUDIOPLAY audioPlay)
		(16r40000106 OSK_AUDIOMUTE audioMute)
		(16r40000107 OSK_MEDIASELECT mediaSelect)
		(16r40000108 OSK_WWW www)
		(16r40000109 OSK_MAIL mail)
		(16r4000010A OSK_CALCULATOR calculator)
		(16r4000010B OSK_COMPUTER computer)
		(16r4000010C OSK_AC_SEARCH ac_search)
		(16r4000010D OSK_AC_HOME ac_home)
		(16r4000010E OSK_AC_BACK ac_back)
		(16r4000010F OSK_AC_FORWARD ac_forward)
		(16r40000110 OSK_AC_STOP ac_stop)
		(16r40000111 OSK_AC_REFRESH ac_refresh)
		(16r40000112 OSK_AC_BOOKMARKS ac_bookmarks)
		(16r40000113 OSK_BRIGHTNESSDOWN brightnessDown)
		(16r40000114 OSK_BRIGHTNESSUP brightnessUp)
		(16r40000115 OSK_DISPLAYSWITCH displaySwitch)
		(16r40000116 OSK_KBDILLUMTOGGLE kbdIllumToggle)
		(16r40000117 OSK_KBDILLUMDOWN kbdIllumDown)
		(16r40000118 OSK_KBDILLUMUP kbdIllumUp)
		(16r40000119 OSK_EJECT eject)
		(16r4000011A OSK_SLEEP sleep)"
	)
]

{ #category : #initialization }
OSKeySymbols class >> initialize [
	"
	self initialize
	"
	OSKeySymbolValueToKeyboardKeyMap := Dictionary new.
	OSKeySymbolNameToValueMap := Dictionary new.
	self data do: [ :each |
		| symbolValue classVarName symbolName keyboardKey |
		symbolValue := each first.
		classVarName := each second.
		symbolName := each third asString asUppercase asSymbol.
		keyboardKey := KeyboardKey named: symbolName.
		(self hasClassVarNamed: classVarName) ifFalse: [
			self addClassVarNamed: classVarName
		].
		self classVarNamed: classVarName put: symbolValue.
		OSKeySymbolValueToKeyboardKeyMap at: symbolValue put: keyboardKey.
		OSKeySymbolNameToValueMap at: symbolName put: symbolValue.
	].

]

{ #category : #accessing }
OSKeySymbols class >> mapKeySymbolNameToValue: keySymbolName [
	^ OSKeySymbolNameToValueMap at: keySymbolName ifAbsent: [ OSK_UNKNOWN ]
]

{ #category : #accessing }
OSKeySymbols class >> mapKeySymbolValueToKeyboardKey: keySymbolValue [
	^ OSKeySymbolValueToKeyboardKeyMap at: keySymbolValue ifAbsent: [ nil ]
]
