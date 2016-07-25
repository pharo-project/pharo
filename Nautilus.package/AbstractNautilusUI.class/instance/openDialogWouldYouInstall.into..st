private
openDialogWouldYouInstall: aMethod into: aClass

	"open a dialog to ask user if he wants to override the existing method or not"
	^ UIManager default confirm: 'Are you sure you want to override the method ', aMethod selector, ' in ', aClass name label: 'Override' 