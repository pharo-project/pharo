"
A PragmaMenuAndShortcutRegistrationItem is an item of a menu or keymap
"
Class {
	#name : #PragmaMenuAndShortcutRegistrationItem,
	#superclass : #Object,
	#instVars : [
		'item',
		'action',
		'parent',
		'order',
		'enabledBlock',
		'label',
		'help',
		'icon',
		'selector',
		'arguments',
		'withSeparatorAfter',
		'target',
		'enabled',
		'group',
		'with',
		'precondition',
		'shortcut',
		'platform',
		'default',
		'category',
		'shortcutName',
		'keyText'
	],
	#category : #MenuRegistration-Core
}

{ #category : #accessing }
PragmaMenuAndShortcutRegistrationItem >> action [
	
	^ action
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistrationItem >> action: aBlock [

	action := aBlock
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistrationItem >> arguments [
	
	^ arguments
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistrationItem >> arguments: anArray [

	arguments := anArray
]

{ #category : #protocol }
PragmaMenuAndShortcutRegistrationItem >> category [

	^ category
]

{ #category : #protocol }
PragmaMenuAndShortcutRegistrationItem >> category: aByteSymbol [ 
	
	category := aByteSymbol
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistrationItem >> default [

	^ default
]

{ #category : #protocol }
PragmaMenuAndShortcutRegistrationItem >> default: aKMModifiedShortcut [ 

	default := aKMModifiedShortcut 
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistrationItem >> enabled [

	^ enabled
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistrationItem >> enabled: aBoolean [

	enabled := aBoolean
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistrationItem >> enabledBlock [
	
	^ enabledBlock
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistrationItem >> enabledBlock: aBlock [

	enabledBlock := aBlock
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistrationItem >> group [
	
	^ group
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistrationItem >> group: aSymbol [

	group := aSymbol
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistrationItem >> help [

	^ help
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistrationItem >> help: aString [ 
	
	help := aString 
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistrationItem >> icon [

	^ icon
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistrationItem >> icon: anIcon [
	
	icon := anIcon
]

{ #category : #initialization }
PragmaMenuAndShortcutRegistrationItem >> initialize [

	super initialize.

	withSeparatorAfter := false.
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistrationItem >> isWithSeparatorAfter [
	
	^ withSeparatorAfter
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistrationItem >> item [

	^ item
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistrationItem >> item: aSymbol [ 
	
	item := aSymbol
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistrationItem >> keyText [
	^keyText
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistrationItem >> keyText: aString [
	keyText := aString
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistrationItem >> label [

	^ label
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistrationItem >> label: aString [
	
	label := aString
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistrationItem >> order [

	^ order
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistrationItem >> order: anInteger [ 

	order := anInteger
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistrationItem >> parent [

	^ parent
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistrationItem >> parent: aSymbol [ 
	
	parent := aSymbol 
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistrationItem >> platform [

	^ platform
]

{ #category : #protocol }
PragmaMenuAndShortcutRegistrationItem >> platform: aSymbol [ 

	platform := aSymbol
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistrationItem >> precondition: aBlock [
	
	precondition := aBlock 
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistrationItem >> selector [

	^ selector
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistrationItem >> selector: aSymbol [ 
	
	selector := aSymbol 
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistrationItem >> shortcut [
	
	^ shortcut
]

{ #category : #protocol }
PragmaMenuAndShortcutRegistrationItem >> shortcut: aSymbol [ 
	
	shortcut := aSymbol
]

{ #category : #protocol }
PragmaMenuAndShortcutRegistrationItem >> shortcutName [
	
	^shortcutName
]

{ #category : #protocol }
PragmaMenuAndShortcutRegistrationItem >> shortcutName: aSymbol [ 
	
	shortcutName := aSymbol
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistrationItem >> target [

	^ target
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistrationItem >> target: aSymbol [ 
	
	target := aSymbol 
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistrationItem >> with [

	^ with
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistrationItem >> with: aBlock [
 
	with := aBlock
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistrationItem >> withSeparatorAfter [

	withSeparatorAfter := true
]
