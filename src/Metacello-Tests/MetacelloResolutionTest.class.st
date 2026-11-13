"
I test MetacelloResolution and MetacelloResolvedProject, with the baseline of TestToLock2 (packages TestToLockPackage1 and TestToLockPackage2) already in the image, so nothing is fetched.
"
Class {
	#name : 'MetacelloResolutionTest',
	#superclass : 'MetacelloTestCase',
	#category : 'Metacello-Tests-Resolution',
	#package : 'Metacello-Tests',
	#tag : 'Resolution'
}

{ #category : 'private' }
MetacelloResolutionTest >> lock2 [
	"TestToLock2, its version built from the baseline in the image"

	| spec |
	Metacello new
		baseline: 'TestToLock2';
		repository: 'http://repo.com';
		register.
	spec := MetacelloProjectRegistration projectSpecForClassNamed: 'TestToLock2' ifAbsent: [ self fail ].
	spec ensureProjectLoadedWithEngine: MetacelloScriptEngine new.
	^ MetacelloResolvedProject projectSpec: spec
]

{ #category : 'private' }
MetacelloResolutionTest >> request: aProject loads: aCollection [
	"A spec asking aProject's project with the groups aCollection"

	^ aProject projectSpec copy
		  loads: aCollection;
		  yourself
]

{ #category : 'private' }
MetacelloResolutionTest >> requester: aName [
	"A resolved project named aName, asking for others"

	^ MetacelloResolvedProject projectSpec: ((MetacelloProjectSpec project: MetacelloProject new)
			   name: aName;
			   className: 'BaselineOf' , aName;
			   yourself)
]

{ #category : 'tests' }
MetacelloResolutionTest >> testAFindingIsAddedToItsRequest [

	| project request finding |
	project := self lock2.
	request := project requestedBy: MetacelloUserRequest new spec: (self request: project loads: #(  )).
	finding := MetacelloResolutionFinding error: #cannotResolve text: 'cannot resolve' request: request.

	self
		assert: request findings asArray equals: { finding };
		assert: finding kind equals: #cannotResolve;
		assert: finding request equals: request
]

{ #category : 'tests' }
MetacelloResolutionTest >> testARequestKnowsTheChainThatLedToIt [

	| root middle leaf request |
	root := self requester: 'Root'.
	root requestedBy: MetacelloUserRequest new spec: (self request: root loads: #( 'Core' )).
	middle := self requester: 'Middle'.
	middle requestedBy: root spec: (self request: middle loads: #( 'Core' )).
	leaf := self lock2.
	request := leaf requestedBy: middle spec: (self request: leaf loads: #(  )).

	self assert: request chain asArray equals: #( 'Root' 'Middle' )
]

{ #category : 'tests' }
MetacelloResolutionTest >> testDefaultGroupsCountAsTheirNames [

	| project |
	project := self lock2.
	project requestedBy: (self requester: 'A') spec: (self request: project loads: #(  )).

	self assertCollection: project loadList hasSameElements: #( 'TestToLockPackage1' 'TestToLockPackage2' )
]

{ #category : 'tests' }
MetacelloResolutionTest >> testFindsProjectsByTheirSpec [

	| resolution project |
	project := self lock2.
	resolution := MetacelloResolution new.
	resolution addRoot: project.

	self assert: resolution roots asArray equals: { project }.
	self assert: (resolution projectFor: project projectSpec ifAbsent: [ nil ]) identicalTo: project.
	self assert: (resolution includes: project projectSpec)
]

{ #category : 'tests' }
MetacelloResolutionTest >> testGroupsAddUp [

	| project |
	project := self lock2.
	project requestedBy: (self requester: 'A') spec: (self request: project loads: #( 'TestToLockPackage1' )).
	project requestedBy: (self requester: 'B') spec: (self request: project loads: #( 'TestToLockPackage2' )).
	project requestedBy: (self requester: 'C') spec: (self request: project loads: #( 'TestToLockPackage1' )).

	self assertCollection: project loadList hasSameElements: #( 'TestToLockPackage1' 'TestToLockPackage2' )
]

{ #category : 'tests' }
MetacelloResolutionTest >> testOnlyErrorsStopALoad [

	| resolution project request |
	project := self lock2.
	request := project requestedBy: MetacelloUserRequest new spec: (self request: project loads: #(  )).
	resolution := MetacelloResolution new.
	resolution addFinding: (MetacelloResolutionFinding warning: #aWarning text: 'a warning' request: request).
	self deny: resolution hasErrors.

	resolution addFinding: (MetacelloResolutionFinding error: #anError text: 'an error' request: MetacelloUserRequest new).
	self assert: resolution hasErrors.
	self assert: (resolution errors collect: [ :each | each text ]) asArray equals: #( 'an error' ).
	self assert: (resolution warnings collect: [ :each | each request projectName ]) asArray equals: #( 'TestToLock2' )
]

{ #category : 'tests' }
MetacelloResolutionTest >> testRequestsKeepWhoAskedWhat [

	| project b |
	project := self lock2.
	b := self requester: 'B'.
	project requestedBy: MetacelloUserRequest new spec: (self request: project loads: #(  )).
	project requestedBy: b spec: (self request: project loads: #( 'TestToLockPackage2' )).
	self
		assert: (project requests collect: [ :each | each requesterName ]) asArray equals: #( 'the script' 'B' );
		assert: (project requests collect: [ :each | each loads asArray ]) asArray equals: #( #(  ) #( 'TestToLockPackage2' ) );
		assert: (project requests collect: [ :each | each projectName ]) asArray equals: #( 'TestToLock2' 'TestToLock2' )
]

{ #category : 'tests' }
MetacelloResolutionTest >> testTheReportGroupsFindingsBySeverityAndKind [
	"Errors and infos are listed, warnings only counted per kind"

	| project request resolution |
	project := self lock2.
	request := project requestedBy: MetacelloUserRequest new spec: (self request: project loads: #(  )).
	resolution := MetacelloResolution new.
	resolution
		addFinding: (MetacelloResolutionFinding error: #cannotResolve text: 'cannot resolve' request: request);
		addFinding: (MetacelloResolutionFinding warning: #oddVersion text: 'odd version' request: request);
		addFinding: (MetacelloResolutionFinding info: #newerVersion text: 'newer version' request: request).

	self assert: resolution report equals: (String lf join:
			 #( '1 errors, 1 warnings, 1 infos' '' 'Errors (1)' '  cannotResolve (1)' '    - cannot resolve' '' 'Warnings (1)'
			    '  oddVersion (1)' '' 'Information (1)' '  newerVersion (1)' '    - newer version' ))
]

{ #category : 'tests' }
MetacelloResolutionTest >> testTheVerboseReportListsWarnings [

	| project request resolution |
	project := self lock2.
	request := project requestedBy: MetacelloUserRequest new spec: (self request: project loads: #(  )).
	resolution := MetacelloResolution new.
	resolution addFinding: (MetacelloResolutionFinding warning: #oddVersion text: 'odd version' request: request).

	self
		assert: resolution verboseReport
		equals: (String lf join: #( '0 errors, 1 warnings, 0 infos' '' 'Warnings (1)' '  oddVersion (1)' '    - odd version' ))
]
