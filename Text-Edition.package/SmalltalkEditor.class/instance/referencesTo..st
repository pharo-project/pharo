menu messages
referencesTo: aVariableOrClassName
	"Open a references browser on the given symbol"
	| env ref |

	" ugly dispatch, but current Browser protocol names aren't really cool "
	env := self modelCurrentSelectedClass ifNil: [ Smalltalk globals ].

	env isBehavior ifTrue: [
		(env instVarIndexFor: aVariableOrClassName) > 0 ifTrue: [
		 ^ self systemNavigation browseAllAccessesTo: aVariableOrClassName from: env]].

	ref:= (env bindingOf: aVariableOrClassName) ifNil: [ ^ self ].
	
	self systemNavigation browseAllReferencesTo: ref