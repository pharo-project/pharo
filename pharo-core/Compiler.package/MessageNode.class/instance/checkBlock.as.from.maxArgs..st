private
checkBlock: node as: nodeName from: encoder maxArgs: maxArgs
	"Answer true if node is a BlockNode with at most maxArgs arguments.
	This check is required in order to inline some special messages.
	Notify some undue usage of these special messages."

	node isBlockNode ifFalse: [ ^false ].
	node numberOfArguments <= maxArgs ifTrue: [ ^true ].
	^encoder notify: '<- ', nodeName , ' of ' , (MacroSelectors at: special) , ' has too many arguments'