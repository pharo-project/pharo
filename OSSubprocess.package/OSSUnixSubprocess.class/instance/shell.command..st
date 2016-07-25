shell
shell: aShellPath command: aShellCommandString
	"Please read comment of #shellCommand:.
	This is the same as shellCommand: but with the addition that the use can define which shell to be used via 'aBasePath' "
	command := aShellPath.
	arguments := Array with: '-c' with: aShellCommandString. 