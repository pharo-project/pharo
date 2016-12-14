!PharoClassInstaller methodsFor: 'migrating' stamp: 'PavelKrivanek 7/19/2016 12:09'!
copyMethodsFrom: oldClass to: newClass using: classModification
	|newClassVariables|
	newClassVariables := classModification classVariables asDictionary.
	oldClass localMethods do: [ :m | | copy |
		copy := m copy.
		copy methodClass: newClass.
		copy literals withIndexDo: [:l :i |
			(l isKindOf: ClassVariable)
				ifTrue: [ | newLiteral |
					newLiteral := classModification newClass innerBindingOf: l key.
					m literalAt: i put: newLiteral ].
		].
		newClass addSelectorSilently: copy selector withMethod: copy.
	].
	oldClass classSide localMethods do: [ :m | | copy |
		copy := m copy.
		copy methodClass: newClass classSide.
		copy literals withIndexDo: [:l :i |
			(l isKindOf: ClassVariable)
				ifTrue: [ | newLiteral |
					newLiteral := classModification newClass innerBindingOf: l key.
					m literalAt: i put: newLiteral ].
		].
		newClass classSide addSelectorSilently: copy selector withMethod: copy.
	].! !



!MCClassDefinition methodsFor: 'installing' stamp: 'MarcusDenker 4/22/2015 19:23'!
createClass
	| superClass |
	
	name = #Context ifTrue: [^ self].
	
	superClass := superclassName == #nil ifFalse:
						[ Smalltalk globals at: superclassName ].
	^[PharoClassInstaller make: [ :builder | 
		builder
			superclass: superClass;
			name: name;
			layoutClass: (ObjectLayout layoutForType: type);
			slots: self instanceVariables;
			sharedVariables: self classVariables;
			sharedPools: self sharedPoolsString;
			classSlots: self classInstanceVariables;
			traitComposition: self traitCompositionCompiled;
			classTraitComposition: self classTraitCompositionCompiled;
			comment: comment stamp: commentStamp;
			category: category;
			environment:  superClass environment]] on: Warning, DuplicatedVariableError do: [:ex| ex resume].
	


! !	
	
	
| packages pharoRepository |

RBProgramNode classPool at: #FormatterClass put: BISimpleFormatter.

Undeclared removeUnreferencedKeys.

MCPackage logCr.

MCCacheRepository reset.

localRepository := MCDirectoryRepository new
	directory: './kernelPackages' asFileReference;
	yourself.	
	
loadVersions := [:versions | 
versions do: [ :each | | mcVersion version |
	
   [ 
      loader := MCVersionLoader new.	
      mcVersion := each basenameWithoutExtension.
      mcVersion logCr..
      version := localRepository versionFrom: mcVersion.
      "(version changes operations collect: [:each | each definition summary]) logCr."
	loader addVersion: version.
	loader load.	
    ] on: MCMergeOrLoadWarning do: [:warning | 
      warning messageText logCr.
      warning resume: true]]].
      
      
loadVersions value: (localRepository directory files sorted reject: [ :f | ((f basename beginsWith: 'Slot-') or: [f basename beginsWith: 'Collections-Sequenceable']) or: [f basename beginsWith: 'Kernel-'] ]).
	
'Reload rest:' logCr.	
	
loadVersions value: (localRepository directory files sorted select: [ :f | f basename beginsWith: 'Kernel-' ]). 


localRepository := MCDirectoryRepository new
	directory: './mcPackages' asFileReference;
	yourself.	
	
localRepository directory files sorted
	do: [ :each | | mcVersion |
	
   [  
      loader := MCVersionLoader new.	
      mcVersion := each basenameWithoutExtension.
      mcVersion logCr..
	loader addVersion: (localRepository versionFrom: mcVersion).
	loader load.	
    ] on: MCMergeOrLoadWarning do: [:warning | 
      warning messageText logCr.
      warning resume: true].
].	

loadVersions value: (localRepository directory files sorted select: [ :f | f basename beginsWith: 'Slot-' ]). 
loadVersions value: (localRepository directory files sorted select: [ :f | f basename beginsWith: 'Collections-Sequenceable' ]). 


!
