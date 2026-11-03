"
`migration script :`
forScript := ExtensionNewSystem new.

forScript createAllLocalExtensionObjects. 

forbiddenPackages := #( 
   'OpalCompiler' 
   'Kernel' 
   'Collections' 
   'Morphic' 
   'UnifiedFFI' 
   'ThreadedFFI' 
   'FreeType' 
	'AST-Core'
	'FileSystem'
	'Rubric'
	'Text'
	'Spec2-Commander2'
).

localExtensionWithNoPragmas := (forScript localExtensionMethods reject: [ :m | (m pragmas isNotEmpty) ]). 

methodsToCompile := localExtensionWithNoPragmas reject: [ :method | 
    forbiddenPackages anySatisfy: [ :forbiddenName | (method package name beginsWith: forbiddenName) or: 
        [ method methodClass package name beginsWith: forbiddenName ] ] 
]. ""size 916""



forScript compileMethodsForCollection: methodsToCompile.
""to separate class already having getter and those which don't""
forScript := ExtensionNewSystem new.  
methodsToAnalyze := forScript localExtensionMethodsWithInstVar.  
easyMethods := OrderedCollection new. 
complexMethods := OrderedCollection new.  

methodsToAnalyze do: [ :method |  
    | targetClass ast isComplex |  
    targetClass := method methodClass.  
    ast := method ast.  
    isComplex := false.  
    
    ast instanceVariableNodes do: [ :node |  
        | varName |  
        varName := node name.  
        
        (targetClass includesSelector: varName asSymbol) ifTrue: [  
            | existingGetterAst firstStatement returnedNode |  
            existingGetterAst := (targetClass lookupSelector: varName asSymbol) ast.  
            
            existingGetterAst statements ifNotEmpty: [ :statements |
                firstStatement := statements first.
                
                (statements size > 1 
                    or: [ firstStatement isReturn not 
                    or: [ returnedNode := firstStatement value.
                          (returnedNode isVariable and: [ returnedNode name = varName ]) not ]]) 
                            ifTrue: [ isComplex := true ]
            ] ifEmpty: [ isComplex := true ].
        ] ifFalse: [ 
            isComplex := true 
        ]
    ].  
    
    isComplex  
        ifTrue: [ complexMethods add: method ]  
        ifFalse: [ easyMethods add: method ] 
].  

{  
    (#easy -> easyMethods).  
    (#complex -> complexMethods) 
} inspect.
"
Class {
	#name : 'ExtensionNewSystem',
	#superclass : 'Object',
	#instVars : [
		'allExtensionMethods',
		'sendersMap',
		'localExtensionMethods',
		'localExtensionByPackage',
		'allExtensionObjectsSymbol',
		'localExtensionMethodsWithInstVar'
	],
	#category : 'Extension-New-System',
	#package : 'Extension-New-System'
}

{ #category : 'accessing' }
ExtensionNewSystem >> allExtensionMethods [

	^ allExtensionMethods
]

{ #category : 'accessing' }
ExtensionNewSystem >> allExtensionObjectsSymbol [

	^ allExtensionObjectsSymbol
]

{ #category : 'initialization' }
ExtensionNewSystem >> compileAllLocalExtensionMethods [

	self compileMethodsForCollection: localExtensionMethods
]

{ #category : 'initialization' }
ExtensionNewSystem >> compileMethodsForCollection: anExtensionMethodsList [

	| cpt |
	cpt := 1.
	SystemAnnouncer uniqueInstance suspendAllWhile: [
			anExtensionMethodsList do: [ :method |
					| nameOfTheExtensionObject targetExtensionClass |
					[
						nameOfTheExtensionObject := self findExtensionObjectNameWithMethod: method.
						targetExtensionClass := Smalltalk at: nameOfTheExtensionObject asSymbol.
						targetExtensionClass compile: method sourceCode classified: 'extensions'.
						method methodClass removeSelector: method selector.
						Stdio stdout
							nextPutAll: cpt asString , ' : ' , method name asString;
							lf;
							flush.
						cpt := cpt + 1 ]
						on: Error
						do: [ :error |
								Stdio stdout
									nextPutAll: 'Migration ignorée pour : ' , method printString , ' ' , error description;
									lf ] ] ].
	Stdio stdout
		nextPutAll: 'installation done';
		lf;
		flush
]

{ #category : 'initialization' }
ExtensionNewSystem >> compileMethodsInTheirExtensionObjectForPackage: aPackage [

	| anExtensionMethodsList |
	anExtensionMethodsList := localExtensionByPackage at: aPackage.
	self compileMethodsForCollection: anExtensionMethodsList
]

{ #category : 'extensionObjects' }
ExtensionNewSystem >> createAllLocalExtensionObjects [

	allExtensionObjectsSymbol := Set new.
	localExtensionMethods do: [ :method |
			| nameOfTheExtensionObject |
			nameOfTheExtensionObject := self findExtensionObjectNameWithMethod: method.
			(allExtensionObjectsSymbol includes: nameOfTheExtensionObject) ifFalse: [
					(Extension << nameOfTheExtensionObject asSymbol)
						extendedClass: method methodClass;
						package: method package name;
						install.
					allExtensionObjectsSymbol add: nameOfTheExtensionObject ] ]
]

{ #category : 'extensionObjects' }
ExtensionNewSystem >> findExtensionObjectNameWithMethod: method [

	| targetClass targetPackage nameOfTheExtensionObject |
	targetClass := method methodClass.
	targetPackage := (method package name copyReplaceAll: '-' with: '') copyReplaceAll: ' ' with: ''.
	nameOfTheExtensionObject := 'ExtensionOf' , targetPackage , 'On' , (targetClass name copyReplaceAll: ' ' with: '').

	^ nameOfTheExtensionObject
]

{ #category : 'initialization' }
ExtensionNewSystem >> generateSendersMap [

	| selectorsOfExtensionMethods |
	selectorsOfExtensionMethods := (allExtensionMethods collect: [ :m | m selector ]) asSet.
	sendersMap := Dictionary new.
	SystemNavigation default allBehaviorsDo: [ :class |
			class methodsDo: [ :aMethod |
					aMethod messages do: [ :msg |
						(selectorsOfExtensionMethods includes: msg) ifTrue: [
							(sendersMap at: msg ifAbsentPut: [ OrderedCollection new ]) add: aMethod ] ] ] ]
]

{ #category : 'initialization' }
ExtensionNewSystem >> initialize [

	| mSenders |
	super initialize.
	allExtensionMethods := PackageOrganizer default packages flatCollect: [ :p | p extensionMethods ].
	self generateSendersMap.
	self localExtensionMethods.
	localExtensionMethods := allExtensionMethods select: [ :m |
			                         mSenders := sendersMap at: m selector ifAbsent: [ #(  ) ].
			                         mSenders allSatisfy: [ :sender | sender package = m package ] ].
	localExtensionByPackage := localExtensionMethods groupedBy: [ :m | m package name ].
	localExtensionMethodsWithInstVar := localExtensionMethods select: [ :m |
		                                    m ast variableNodes anySatisfy: [ :node | node isInstanceVariable ] ]
]

{ #category : 'accessing' }
ExtensionNewSystem >> localExtensionByPackage [

	^ localExtensionByPackage
]

{ #category : 'initialization' }
ExtensionNewSystem >> localExtensionMethods [

	^ localExtensionMethods
]

{ #category : 'as yet unclassified' }
ExtensionNewSystem >> localExtensionMethodsWithInstVar [

	^ localExtensionMethodsWithInstVar
]

{ #category : 'extensionObjects' }
ExtensionNewSystem >> removeAllLocalExtensionObjects [

	allExtensionObjectsSymbol
		do: [ :o | (Smalltalk at: o asSymbol) removeFromSystem ];
		removeAll
]

{ #category : 'accessing' }
ExtensionNewSystem >> sendersMap [

	^ sendersMap
]
