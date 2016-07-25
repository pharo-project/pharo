initialize-release
initializeSounds
	"Initialize the receiver's event sounds."

	self sounds: Dictionary new.
	self sounds
		at: #default put: self defaultDefaultSound;
		at: #abort put: self defaultAbortSound;
		at: #alert put: self defaultAlertSound;
		at: #deny put: self defaultDenySound;
		at: #message put: self defaultMessageSound;
		at: #question put: self defaultQuestionSound;
		at: #windowMinimize put: self defaultWindowMinimizeSound;
		at: #windowMaximize put: self defaultWindowMaximizeSound;
		at: #windowRestoreUp put: self defaultWindowRestoreUpSound;
		at: #windowRestoreDown put: self defaultWindowRestoreDownSound;
		at: #windowClose put: self defaultWindowCloseSound