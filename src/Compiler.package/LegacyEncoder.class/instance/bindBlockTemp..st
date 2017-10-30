temps
bindBlockTemp: name 
	"Declare a temporary block variable; complain if it's not a field or class variable."

	| node |

	node := scopeTable at: name ifAbsent: [^self reallyBind: name].
	node isTemp
		ifTrue: [
			node scope >= 0 ifTrue: [^ self notify: 'Name already used in this method'].
			node scope: 0]
		ifFalse: [^self notify: 'Name already used in this class'].
	^node
