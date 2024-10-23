Extension { #name : 'CmdShortcutActivation' }

{ #category : '*Keymapping-Tools-Spec' }
CmdShortcutActivation >> description [

	^ String empty
]

{ #category : '*Keymapping-Tools-Spec' }
CmdShortcutActivation >> name [

	^ self declarationSelector asString
]

{ #category : '*Keymapping-Tools-Spec' }
CmdShortcutActivation >> scope [
	"The receiver's scope is the package name where is installed"

	^ self annotatedClass packageName
]

{ #category : '*Keymapping-Tools-Spec' }
CmdShortcutActivation >> scopeName [

	^ self scope asString
]

{ #category : '*Keymapping-Tools-Spec' }
CmdShortcutActivation >> shortcut [

	^ self keyCombination asString
]
