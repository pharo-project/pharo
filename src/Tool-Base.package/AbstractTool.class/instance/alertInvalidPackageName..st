package
alertInvalidPackageName:name
	| errorMessage |

	(name isNil or: [ name isEmpty ]) ifTrue: [ ^ self ]. "User cancelled the action"
	
	errorMessage := String streamContents: [:stream |
		stream << name << ' is not a valid package name.
A package name can not begin with a *' ].
	
	UIManager default inform: errorMessage