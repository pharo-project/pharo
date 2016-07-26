initialize-release
initializePresenter

	showPassword 
		whenActivatedDo: [ password beDecrypted ];
		whenDeactivatedDo: [ password beEncrypted ]