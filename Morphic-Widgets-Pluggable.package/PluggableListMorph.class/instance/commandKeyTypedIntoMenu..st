model access
commandKeyTypedIntoMenu: evt
	"The user typed a command-key into a menu which has me as its command-key handler"

	^ self modifierKeyPressed: evt