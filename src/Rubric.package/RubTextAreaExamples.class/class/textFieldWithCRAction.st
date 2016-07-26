examples
textFieldWithCRAction
	"self textFieldWithCRAction"

	| s |
	s := RubTextFieldArea new
		width: 800;
		setTextWith: 'My contents will be printed in the Transcript when return is entered';
		forbidMenu;
		openInWorld.
	s announcer when: RubReturnEntered send: #value to:  [ Transcript show: s text asString ]