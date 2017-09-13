private
addRuleRemoving: patternString
	"When you are completely removing statement(s), you can not just match the relevant node. You must match the whole method and then replace it minus the part to be removed"

	| findString replaceString methodTemplate |
	methodTemplate := '| `@Temps |
``@.Statements1.
{1}.
``@.Statements2'.
	findString := methodTemplate format: { patternString }.
	replaceString := methodTemplate format: { '' }.
	self replace: findString with: replaceString