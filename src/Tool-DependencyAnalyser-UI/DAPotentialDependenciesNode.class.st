"
I'm a node allowing to compute and browse potential dependencies of a package, i.e. messages sent in the package but with no corresponding method in the package or in its dependencies.
"
Class {
	#name : #DAPotentialDependenciesNode,
	#superclass : #DADependentPackageNode,
	#instVars : [
		'messageSendAnalysis'
	],
	#category : #'Tool-DependencyAnalyser-UI-Nodes'
}

{ #category : #api }
DAPotentialDependenciesNode >> buildChildren [
	^ (self messageSendAnalysis unimplementedCallsInAllDependencies
		collect: [ :selector | 
						(DAMissingMethodNode new
								content: selector;
								parentNode: self;
								yourself) ]
		as: Array) 
			sort: [ :a :b | a content < b content ]
]

{ #category : #accessing }
DAPotentialDependenciesNode >> content [
	^ 'Potential dependencies'
]

{ #category : #accessing }
DAPotentialDependenciesNode >> extensionDependenciesSize [
	^ 0
]

{ #category : #accessing }
DAPotentialDependenciesNode >> hasChildren [
	^ true
]

{ #category : #accessing }
DAPotentialDependenciesNode >> icon [
	^ self iconNamed: #smallHelpIcon
]

{ #category : #testing }
DAPotentialDependenciesNode >> isPackageNode [
	^ false
]

{ #category : #displaying }
DAPotentialDependenciesNode >> label [
	^ self content
]

{ #category : #accessing }
DAPotentialDependenciesNode >> messageSendAnalysis [
	[ messageSendAnalysis ifNil: [ messageSendAnalysis := DAMessageSendAnalyzer on: self parentNode content packageName ] ]
		on: PotentialOutDatedDependencyWarning 
		do: [ :ex | ex resume. "ignore. We already got an alert box" ].
	^ messageSendAnalysis
]

{ #category : #compatibility }
DAPotentialDependenciesNode >> numberOfDependencies [
	^ 0
]

{ #category : #'browse-nautilus' }
DAPotentialDependenciesNode >> selectInBrowser [
	self selectMethod:
		([ self packageUnderAnalysis packageManifest class methodNamed: #manuallyResolvedDependencies ]
			on: KeyNotFound
			do: [ PackageManifest class>>#manuallyResolvedDependencies ])
]
