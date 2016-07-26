window menu
warnUser
	"Warn the user about the possible content loss"

	^ UIManager default confirm: 'This will replace the current workspace content.', String cr, 'Do you want to continue?' translated