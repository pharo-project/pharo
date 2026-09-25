Class {
	#name : 'OCExtensionMethodInstaller',
	#superclass : 'OCAnnouncingMethodInstaller',
	#instVars : [
		'shouldRecompileDependencies'
	],
	#category : 'Kernel-CodeModel-Methods',
	#package : 'Kernel-CodeModel',
	#tag : 'Methods'
}

{ #category : 'initialization' }
OCExtensionMethodInstaller >> initialize [

	super initialize.
	shouldRecompileDependencies := false
]

{ #category : 'installing' }
OCExtensionMethodInstaller >> install: method into: extension compiler: compiler [

	| literalIndex scopedSelector visibleExtensions methodCopyInExtendedClass selectorSymbol packagesToRecompile existingMethod isLegacyExtension |

	selectorSymbol := method selector.
	scopedSelector := extension internScopedSelector: selectorSymbol.
	
	"If this is the first appearance of the selector, we just created it, and we need to patch the selector."
	literalIndex := method literals indexOf: selectorSymbol.
	
	"We need to set the selector, which is sometimes in a method properties and sometimes in the literal frame.
	Thus, patching the literal will not be enough."
	method selector: scopedSelector.
	literalIndex > 0 ifTrue: [
		"Then, patching the literal should fix also sends to this selector in the method"
		method literalAt: literalIndex put: scopedSelector ].
	"Also set the extension class into the method.
	The method belongs to the extension and not to the extended class."
	method methodClass: extension.
	
	super install: method into: extension compiler: compiler.
	
	"If the extension overwrites a normal method, then install a conflict method"
	visibleExtensions := scopedSelector extensionsOn: extension extendedClass.
	
	
	existingMethod := extension extendedClass methodDict at: selectorSymbol ifAbsent: [ nil ].
	isLegacyExtension := Extension allowMigration and: [existingMethod notNil and: [ existingMethod isExtension ]].

	methodCopyInExtendedClass := ((extension extendedClass methodDictionary includesKey: selectorSymbol) and: [ isLegacyExtension not ])
    										ifTrue: [ 
        										ConflictMethod new
            										overwrittenSelector: selectorSymbol;
            										conflictingClass: extension extendedClass;
            										conflictingExtensions: visibleExtensions;
            										yourself ]
		                             ifFalse: [
				                             visibleExtensions size > 1
					                             ifTrue: [
							                             ConflictMethod new
								                             overwrittenSelector: selectorSymbol;
								                             conflictingClass: extension extendedClass;
								                             conflictingExtensions: visibleExtensions;
								                             yourself ]
					                             ifFalse: [
							                             method copy
								                             methodClass: extension extendedClass;
								                             yourself ] ]. "We install the extension in the extended class"
	methodCopyInExtendedClass propertyAt: #extensionPackage put: extension package.
	extension extendedClass classify: scopedSelector under: protocol.
	extension extendedClass methodDict at: scopedSelector put: methodCopyInExtendedClass.
	scopedSelector addExtendedBehavior: extension extendedClass. "Alias subclasses"
	scopedSelector bindAliasesInSubclassesOf: extension extendedClass. "Update user packages so they define a locally scoped selector with the same name"
	extension hasUserPackages ifTrue: [ extension userPackages do: [ :e | e internScopedSelector: selectorSymbol ] ].

	shouldRecompileDependencies ifFalse: [ ^ self ].
	packagesToRecompile := { extension package }.
	extension hasUserPackages ifTrue: [ packagesToRecompile := packagesToRecompile , extension userPackages asArray ].
	(packagesToRecompile flatCollect: [ :p | p methods ]) do: [ :m |
			m methodClass compile: m sourceCode methodInstaller: (m methodClass methodInstaller
					 logged: false;
					 changeStamp: changeStamp;
					 protocol: m protocol) ]
]

{ #category : 'accessing' }
OCExtensionMethodInstaller >> shouldRecompileDependencies [

	^ shouldRecompileDependencies
]

{ #category : 'accessing' }
OCExtensionMethodInstaller >> shouldRecompileDependencies: anObject [

	shouldRecompileDependencies := anObject
]
