Class {
	#name : #MetacelloPackagesSpecTestCase,
	#superclass : #MetacelloSpecTestCase,
	#category : #'Metacello-TestsCore-Specs'
}

{ #category : #tests }
MetacelloPackagesSpecTestCase >> testAddGroupA [
    "used by MetacelloAbstractVersionConstructor>>group:overrides:"

    | packages group |
    packages := self packagesSpec.
    packages
        add:
            (self groupSpec
                name: 'Platform';
                includes: 'Core';
                yourself).
    packages
        add:
            (self groupSpec
                name: 'Platform';
                includes: 'Tests';
                yourself).
    group := packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
    self deny: (group includes includes: 'Core').
    self assert: (group includes includes: 'Tests')
]

{ #category : #tests }
MetacelloPackagesSpecTestCase >> testAddGroupB [
    "used by MetacelloAbstractVersionConstructor>>group:overrides:"

    | packages group |
    packages := self packagesSpec.
    packages
        add:
            {(self groupSpec
                name: 'Platform';
                includes: 'Core';
                yourself).
            (self groupSpec
                name: 'Platform';
                includes: 'Tests';
                yourself)}.
    group := packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
    self deny: (group includes includes: 'Core').
    self assert: (group includes includes: 'Tests')
]

{ #category : #tests }
MetacelloPackagesSpecTestCase >> testAddProjectA [
	"used by MetacelloAbstractVersionConstructor>>project:overrides:"

	| packages project projectReferenceSpec |
	packages := self packagesSpec.
	packages
		add:
			(self projectSpec
				name: 'Project';
				className: 'ConfigurationOfProjectA';
				versionString: #stable;
				loads: #('MyPackage' 'MyTests');
				preLoadDoIt: #preLoadDoItB;
				postLoadDoIt: #postLoadDoItB;
				yourself).
	packages
		add:
			(self projectSpec
				name: 'Project';
				className: 'ConfigurationOfProject';
				versionString: '1.0';
				operator: #<;
				loads: #('MyPackage');
				preLoadDoIt: #preLoadDoIt;
				postLoadDoIt: #postLoadDoIt;
				yourself).
	projectReferenceSpec := packages packageNamed: 'Project' ifAbsent: [ self assert: false ].
	self assert: projectReferenceSpec projectName equals: 'Project'.
	self assert: projectReferenceSpec versionString equals: '1.0'.
	self should: [ projectReferenceSpec includes: #() ] raise: Error.
	self should: [ projectReferenceSpec requires: #() ] raise: Error.
	self should: [ projectReferenceSpec answers: #() ] raise: Error.
	projectReferenceSpec
		projectDo: [ :prjct | self assert: projectReferenceSpec identicalTo: prjct ]
		packageDo: [ :ignored | self assert: false ]
		groupDo: [ :ignored | self assert: false ].
	project := projectReferenceSpec referencedSpec.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'ConfigurationOfProject'.
	self assert: project versionString equals: '1.0'.
	self assert: project operator identicalTo: #<.
	self assert: project loads equals: #('MyPackage').
	self assert: project preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoIt.
	project projectDo: [ :prjct | self assert: project identicalTo: prjct ] packageDo: [ :ignored | self assert: false ] groupDo: [ :ignored | self assert: false ]
]

{ #category : #tests }
MetacelloPackagesSpecTestCase >> testAddProjectB [
	"used by MetacelloAbstractVersionConstructor>>project:overrides:"

	| packages project projectReferenceSpec |
	packages := self packagesSpec.
	packages
		add:
			{(self projectSpec
				name: 'Project';
				className: 'ConfigurationOfProjectA';
				versionString: #stable;
				loads: #('MyPackage' 'MyTests');
				preLoadDoIt: #preLoadDoItB;
				postLoadDoIt: #postLoadDoItB;
				yourself) . (self projectSpec
				name: 'Project';
				className: 'ConfigurationOfProject';
				versionString: '1.0';
				operator: #<;
				loads: #('MyPackage');
				preLoadDoIt: #preLoadDoIt;
				postLoadDoIt: #postLoadDoIt;
				yourself)}.
	projectReferenceSpec := packages packageNamed: 'Project' ifAbsent: [ self assert: false ].
	self assert: projectReferenceSpec projectName equals: 'Project'.
	self assert: projectReferenceSpec versionString equals: '1.0'.
	self should: [ projectReferenceSpec includes: #() ] raise: Error.
	self should: [ projectReferenceSpec requires: #() ] raise: Error.
	self should: [ projectReferenceSpec answers: #() ] raise: Error.
	projectReferenceSpec
		projectDo: [ :prjct | self assert: projectReferenceSpec identicalTo: prjct ]
		packageDo: [ :ignored | self assert: false ]
		groupDo: [ :ignored | self assert: false ].
	project := projectReferenceSpec referencedSpec.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'ConfigurationOfProject'.
	self assert: project versionString equals: '1.0'.
	self assert: project operator identicalTo: #<.
	self assert: project loads equals: #('MyPackage').
	self assert: project preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoIt.
	project projectDo: [ :prjct | self assert: project identicalTo: prjct ] packageDo: [ :ignored | self assert: false ] groupDo: [ :ignored | self assert: false ]
]

{ #category : #tests }
MetacelloPackagesSpecTestCase >> testCopyToGroup [
    "not currently used by MetacelloAbstractVersionConstructor"

    | packages group |
    packages := self packagesSpec.
    packages
        add:
            (self groupSpec
                name: 'Platform';
                includes: 'Core';
                yourself).
    packages
        merge:
            (self groupSpec
                name: 'Platform';
                includes: 'Tests';
                yourself).
    group := packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
    self assert: (group includes includes: 'Core').
    self assert: (group includes includes: 'Tests').
    group := self groupSpec
        name: 'PlatformCopy';
        includes: 'Copy';
        yourself.
    packages copy: 'Platform' to: group.
    group := packages packageNamed: 'PlatformCopy' ifAbsent: [ self assert: false ].
    self assert: (group includes includes: 'Core').
    self assert: (group includes includes: 'Copy').
    self assert: (group includes includes: 'Tests')
]

{ #category : #tests }
MetacelloPackagesSpecTestCase >> testCopyToProject [
	"used by MetacelloAbstractVersionConstructor>>project:copyFrom:with:"

	| packages project referenceSpec |
	packages := self packagesSpec.
	packages
		add:
			(self projectSpec
				name: 'Project';
				className: 'ConfigurationOfProjectA';
				versionString: #stable;
				loads: #('MyPackage' 'MyTests');
				preLoadDoIt: #preLoadDoItB;
				postLoadDoIt: #postLoadDoItB;
				yourself).
	project := self projectSpec
		name: 'ProjectCopy';
		yourself.
	referenceSpec := self project projectReferenceSpec
		name: 'ProjectCopy';
		projectReference: project;
		yourself.
	packages copy: 'Project' to: referenceSpec.
	project := (packages packageNamed: 'ProjectCopy' ifAbsent: [ self assert: false ]) referencedSpec.
	self assert: project name equals: 'ProjectCopy'.
	self assert: project className equals: 'ConfigurationOfProjectA'.
	self assert: project versionString equals: #stable.
	self assert: project operator identicalTo: #>=.
	self assert: project loads equals: #('MyPackage' 'MyTests').
	self assert: project preLoadDoIt value identicalTo: #preLoadDoItB.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoItB
]

{ #category : #tests }
MetacelloPackagesSpecTestCase >> testMergeGroupA [
    "used by MetacelloAbstractVersionConstructor>>group:with:"

    | packages group |
    packages := self packagesSpec.
    packages
        add:
            (self groupSpec
                name: 'Platform';
                includes: 'Core';
                yourself).
    packages
        merge:
            (self groupSpec
                name: 'Platform';
                includes: 'Tests';
                yourself).
    group := packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
    self assert: (group includes includes: 'Core').
    self assert: (group includes includes: 'Tests')
]

{ #category : #tests }
MetacelloPackagesSpecTestCase >> testMergeGroupB [
    "used by MetacelloAbstractVersionConstructor>>group:with:"

    | packages group |
    packages := self packagesSpec.
    packages
        add:
            (self groupSpec
                name: 'Platform';
                includes: 'Core';
                yourself).
    packages
        merge:
            {(self groupSpec
                name: 'Platform';
                includes: 'Tests';
                yourself)}.
    group := packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
    self assert: (group includes includes: 'Core').
    self assert: (group includes includes: 'Tests')
]

{ #category : #tests }
MetacelloPackagesSpecTestCase >> testMergeProjectA [
	"used by MetacelloAbstractVersionConstructor>>project:with:"

	| packages project projectReferenceSpec referenceSpec |
	packages := self packagesSpec.
	project := self projectSpec
		name: 'Project';
		className: 'ConfigurationOfProjectA';
		versionString: #stable;
		loads: #('MyPackage' 'MyTests');
		preLoadDoIt: #preLoadDoItB;
		postLoadDoIt: #postLoadDoItB;
		yourself.
	referenceSpec := self project projectReferenceSpec
		name: project name;
		projectReference: project;
		yourself.
	packages add: referenceSpec.
	project := self projectSpec
		name: 'Project';
		className: 'ConfigurationOfProject';
		versionString: '1.0';
		operator: #<;
		loads: #('MyPackage');
		preLoadDoIt: #preLoadDoIt;
		postLoadDoIt: #postLoadDoIt;
		yourself.
	referenceSpec := self project projectReferenceSpec
		name: project name;
		projectReference: project;
		yourself.
	packages merge: referenceSpec.
	projectReferenceSpec := packages packageNamed: 'Project' ifAbsent: [ self assert: false ].
	project := projectReferenceSpec referencedSpec.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'ConfigurationOfProject'.
	self assert: project versionString equals: '1.0'.
	self assert: project operator identicalTo: #<.
	self assert: project loads equals: #('MyPackage').
	self assert: project preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoIt.
	project projectDo: [ :prjct | self assert: project identicalTo: prjct ] packageDo: [ :ignored | self assert: false ] groupDo: [ :ignored | self assert: false ]
]

{ #category : #tests }
MetacelloPackagesSpecTestCase >> testMergeProjectB [
	"used by MetacelloAbstractVersionConstructor>>project:with:"

	| packages project projectReferenceSpec referenceSpec |
	packages := self packagesSpec.
	project := self projectSpec
		name: 'Project';
		className: 'ConfigurationOfProjectA';
		versionString: #stable;
		loads: #('MyPackage' 'MyTests');
		preLoadDoIt: #preLoadDoItB;
		postLoadDoIt: #postLoadDoItB;
		yourself.
	referenceSpec := self project projectReferenceSpec
		name: project name;
		projectReference: project;
		yourself.
	packages add: referenceSpec.
	project := self projectSpec
		name: 'Project';
		className: 'ConfigurationOfProject';
		versionString: '1.0';
		operator: #<;
		loads: #('MyPackage');
		preLoadDoIt: #preLoadDoIt;
		postLoadDoIt: #postLoadDoIt;
		yourself.
	referenceSpec := self project projectReferenceSpec
		name: project name;
		projectReference: project;
		yourself.
	packages merge: {referenceSpec}.
	projectReferenceSpec := packages packageNamed: 'Project' ifAbsent: [ self assert: false ].
	project := projectReferenceSpec referencedSpec.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'ConfigurationOfProject'.
	self assert: project versionString equals: '1.0'.
	self assert: project operator identicalTo: #<.
	self assert: project loads equals: #('MyPackage').
	self assert: project preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoIt.
	project projectDo: [ :prjct | self assert: project identicalTo: prjct ] packageDo: [ :ignored | self assert: false ] groupDo: [ :ignored | self assert: false ]
]

{ #category : #tests }
MetacelloPackagesSpecTestCase >> testRemoveGroupA [
    "used by MetacelloAbstractVersionConstructor>>removeGroup:"

    | packages group removed |
    packages := self packagesSpec.
    packages
        add:
            (self groupSpec
                name: 'Platform';
                includes: 'Core';
                yourself).
    packages
        merge:
            (self groupSpec
                name: 'Platform';
                includes: 'Tests';
                yourself).
    group := packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
    self assert: (group includes includes: 'Core').
    self assert: (group includes includes: 'Tests').
    packages
        remove:
            (self groupSpec
                name: 'Platform';
                includes: 'Core';
                yourself).
    removed := false.
    packages packageNamed: 'Platform' ifAbsent: [ removed := true ].
    self assert: removed
]

{ #category : #tests }
MetacelloPackagesSpecTestCase >> testRemoveGroupB [
    "used by MetacelloAbstractVersionConstructor>>removeGroup:"

    | packages removed |
    packages := self packagesSpec.
    packages
        add:
            {(self groupSpec
                name: 'Platform';
                includes: 'Core';
                yourself).
            (self groupSpec
                name: 'Base';
                includes: 'Base';
                yourself).
            (self groupSpec
                name: 'Tests';
                includes: 'Tests';
                yourself)}.
    packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
    packages packageNamed: 'Base' ifAbsent: [ self assert: false ].
    packages packageNamed: 'Tests' ifAbsent: [ self assert: false ].
    packages
        remove:
            {'Base'.
            'Tests'}.
    packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
    removed := false.
    packages packageNamed: 'Base' ifAbsent: [ removed := true ].
    self assert: removed.
    removed := false.
    packages packageNamed: 'Tests' ifAbsent: [ removed := true ].
    self assert: removed
]

{ #category : #tests }
MetacelloPackagesSpecTestCase >> testRemoveGroupC [
    "used by MetacelloAbstractVersionConstructor>>removeGroup:"

    | packages removed |
    packages := self packagesSpec.
    packages
        add:
            {(self groupSpec
                name: 'Platform';
                includes: 'Core';
                yourself).
            (self groupSpec
                name: 'Base';
                includes: 'Base';
                yourself).
            (self groupSpec
                name: 'Tests';
                includes: 'Tests';
                yourself)}.
    packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
    packages packageNamed: 'Base' ifAbsent: [ self assert: false ].
    packages packageNamed: 'Tests' ifAbsent: [ self assert: false ].
    packages remove: 'Tests'.
    packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
    packages packageNamed: 'Base' ifAbsent: [ self assert: false ].
    removed := false.
    packages packageNamed: 'Tests' ifAbsent: [ removed := true ].
    self assert: removed
]

{ #category : #tests }
MetacelloPackagesSpecTestCase >> testRemoveProjectA [
	"used by MetacelloAbstractVersionConstructor>>removeProject:"

	| packages project projectReferenceSpec removed |
	packages := self packagesSpec.
	packages
		add:
			(self projectSpec
				name: 'Project';
				className: 'ConfigurationOfProject';
				versionString: '1.0';
				operator: #<;
				loads: #('MyPackage');
				preLoadDoIt: #preLoadDoIt;
				postLoadDoIt: #postLoadDoIt;
				yourself).
	projectReferenceSpec := packages packageNamed: 'Project' ifAbsent: [ self assert: false ].
	project := projectReferenceSpec referencedSpec.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'ConfigurationOfProject'.
	self assert: project versionString equals: '1.0'.
	self assert: project operator identicalTo: #<.
	self assert: project loads equals: #('MyPackage').
	self assert: project preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoIt.
	packages
		remove:
			(self projectReferenceSpec
				name: 'Project';
				yourself).
	removed := false.
	packages packageNamed: 'Project' ifAbsent: [ removed := true ].
	self assert: removed
]

{ #category : #tests }
MetacelloPackagesSpecTestCase >> testRemoveProjectB [
	"used by MetacelloAbstractVersionConstructor>>removeProject:"

	| packages project projectReferenceSpec removed |
	packages := self packagesSpec.
	packages
		add:
			(self projectSpec
				name: 'Project';
				className: 'ConfigurationOfProject';
				versionString: '1.0';
				operator: #<;
				loads: #('MyPackage');
				preLoadDoIt: #preLoadDoIt;
				postLoadDoIt: #postLoadDoIt;
				yourself).
	projectReferenceSpec := packages packageNamed: 'Project' ifAbsent: [ self assert: false ].
	project := projectReferenceSpec referencedSpec.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'ConfigurationOfProject'.
	self assert: project versionString equals: '1.0'.
	self assert: project operator identicalTo: #<.
	self assert: project loads equals: #('MyPackage').
	self assert: project preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoIt.
	packages
		remove:
			{(self projectReferenceSpec
				name: 'Project';
				yourself)}.
	removed := false.
	packages packageNamed: 'Project' ifAbsent: [ removed := true ].
	self assert: removed
]
