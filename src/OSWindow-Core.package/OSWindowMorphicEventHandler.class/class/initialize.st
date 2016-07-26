as yet unclassified
initialize
	"
	self initialize
	"
	super initialize.
	SymbolCharacterMapping := Dictionary new.
	{
	 OSK_RETURN . Character cr.
	 OSK_BACKSPACE . Character backspace.
	 OSK_TAB . Character tab.
	 OSK_HOME . Character home.
	 OSK_LEFT . Character arrowLeft.
	 OSK_UP . Character arrowUp.
	 OSK_RIGHT . Character arrowRight.
	 OSK_DOWN . Character arrowDown.
	 OSK_END . Character end.
	 OSK_INSERT . Character insert.
	 OSK_PAGEUP . Character pageUp.
	 OSK_PAGEDOWN . Character pageDown.
	 OSK_DELETE. Character delete.
	
	 OSK_KP_0 . $0.
	 OSK_KP_1 . $1.
	 OSK_KP_2 . $2.
	 OSK_KP_3 . $3.
	 OSK_KP_4 . $4.
	 OSK_KP_5 . $5.
	 OSK_KP_6 . $6.
	 OSK_KP_7 . $7.
	 OSK_KP_8 . $8.
	 OSK_KP_9 . $9.
	 OSK_KP_DIVIDE . $/.
	 OSK_KP_MULTIPLY . $*.
	 OSK_KP_PLUS . $+.
	 OSK_KP_MINUS . $-.
	 OSK_KP_ENTER . Character cr.
	 OSK_KP_PERIOD . $..
	} pairsDo: [ :key :val | SymbolCharacterMapping at: key put: val charCode ]