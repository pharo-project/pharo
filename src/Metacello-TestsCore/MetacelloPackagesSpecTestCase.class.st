Class {
	#name : 'MetacelloPackagesSpecTestCase',
	#superclass : 'MetacelloSpecTestCase',
	#category : 'Metacello-TestsCore-Specs',
	#package : 'Metacello-TestsCore',
	#tag : 'Specs'
}

{ #category : 'tests' }
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

{ #category : 'tests' }
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

{ #category : 'tests' }
MetacelloPackagesSpecTestCase >> testAddPackageA [
	"used by MetacelloAbstractVersionConstructor>>package: and MetacelloAbstractVersionConstructor>>package:overrides:"

	| packages package |
	packages := self packagesSpec.
	packages
		add:
			(self packageSpec
				name: 'Package';
				requires: 'AnotherPackage';
				includes: 'IncludedPackage';
				preLoadDoIt: #preLoadDoIt;
				postLoadDoIt: #postLoadDoIt;
				yourself).
	packages
		add:
			(self packageSpec
				name: 'Package';
				requires: 'AndAnotherPackage';
				includes: 'AndIncludedPackage';
				yourself).
	package := packages packageNamed: 'Package' ifAbsent: [ self assert: false ].
	self assert: package name equals: 'Package'.
	self assert: package requires equals: #('AndAnotherPackage').
	self assert: package includes equals: #('AndIncludedPackage').
	self assert: package preLoadDoIt value identicalTo: nil.
	self assert: package postLoadDoIt value identicalTo: nil
]

{ #category : 'tests' }
MetacelloPackagesSpecTestCase >> testAddPackageB [
	"used by MetacelloAbstractVersionConstructor>>package: and MetacelloAbstractVersionConstructor>>package:overrides:"

	| packages package |
	packages := self packagesSpec.
	packages
		add:
			{(self packageSpec
				name: 'Package';
				requires: 'AnotherPackage';
				includes: 'IncludedPackage';
				preLoadDoIt: #preLoadDoIt;
				postLoadDoIt: #postLoadDoIt;
				yourself) . (self packageSpec
				name: 'Package';
				requires: 'AndAnotherPackage';
				includes: 'AndIncludedPackage';
				yourself)}.
	package := packages packageNamed: 'Package' ifAbsent: [ self assert: false ].
	self assert: package name equals: 'Package'.
	self assert: package requires equals: #('AndAnotherPackage').
	self assert: package includes equals: #('AndIncludedPackage').
	self assert: package preLoadDoIt value identicalTo: nil.
	self assert: package postLoadDoIt value identicalTo: nil
]

{ #category : 'tests' }
MetacelloPackagesSpecTestCase >> testAddPackageC [
    "used by MetacelloAbstractVersionConstructor>>package: and MetacelloAbstractVersionConstructor>>package:overrides:"

    | packages |
    packages := self packagesSpec.
    packages
        add:
            {'Platform'.
            'Base'.
            'Tests'}.
    packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
    packages packageNamed: 'Base' ifAbsent: [ self assert: false ].
    packages packageNamed: 'Tests' ifAbsent: [ self assert: false ]
]

{ #category : 'tests' }
MetacelloPackagesSpecTestCase >> testAddPackageD [
    "used by MetacelloAbstractVersionConstructor>>package: and MetacelloAbstractVersionConstructor>>package:overrides:"

    | packages |
    packages := self packagesSpec.
    packages add: 'Platform'.
    packages packageNamed: 'Platform' ifAbsent: [ self assert: false ]
]

{ #category : 'tests' }
MetacelloPackagesSpecTestCase >> testAddProjectA [
	"used by MetacelloAbstractVersionConstructor>>project:overrides:"

	| packages project projectReferenceSpec |
	packages := self packagesSpec.
	packages
		add:
			(self projectSpec
				name: 'Project';
				className: 'ConfigurationOfProjectA';
				loads: #('MyPackage' 'MyTests');
				preLoadDoIt: #preLoadDoItB;
				postLoadDoIt: #postLoadDoItB;
				yourself).
	packages
		add:
			(self projectSpec
				name: 'Project';
				className: 'ConfigurationOfProject';
				operator: #<;
				loads: #('MyPackage');
				preLoadDoIt: #preLoadDoIt;
				postLoadDoIt: #postLoadDoIt;
				yourself).
	projectReferenceSpec := packages packageNamed: 'Project' ifAbsent: [ self assert: false ].
	self assert: projectReferenceSpec projectName equals: 'Project'.
	self should: [ projectReferenceSpec includes: #() ] raise: Error.
	self should: [ projectReferenceSpec requires: #() ] raise: Error.
	projectReferenceSpec
		projectDo: [ :prjct | self assert: projectReferenceSpec identicalTo: prjct ]
		packageDo: [ :ignored | self assert: false ]
		groupDo: [ :ignored | self assert: false ].
	project := projectReferenceSpec referencedSpec.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'ConfigurationOfProject'.
	self assert: project operator identicalTo: #<.
	self assert: project loads equals: #('MyPackage').
	self assert: project preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoIt.
	project projectDo: [ :prjct | self assert: project identicalTo: prjct ] packageDo: [ :ignored | self assert: false ] groupDo: [ :ignored | self assert: false ]
]

{ #category : 'tests' }
MetacelloPackagesSpecTestCase >> testAddProjectB [
	"used by MetacelloAbstractVersionConstructor>>project:overrides:"

	| packages project projectReferenceSpec |
	packages := self packagesSpec.
	packages
		add:
			{(self projectSpec
				name: 'Project';
				className: 'ConfigurationOfProjectA';
				loads: #('MyPackage' 'MyTests');
				preLoadDoIt: #preLoadDoItB;
				postLoadDoIt: #postLoadDoItB;
				yourself) . (self projectSpec
				name: 'Project';
				className: 'ConfigurationOfProject';
				operator: #<;
				loads: #('MyPackage');
				preLoadDoIt: #preLoadDoIt;
				postLoadDoIt: #postLoadDoIt;
				yourself)}.
	projectReferenceSpec := packages packageNamed: 'Project' ifAbsent: [ self assert: false ].
	self assert: projectReferenceSpec projectName equals: 'Project'.
	self should: [ projectReferenceSpec includes: #() ] raise: Error.
	self should: [ projectReferenceSpec requires: #() ] raise: Error.
	projectReferenceSpec
		projectDo: [ :prjct | self assert: projectReferenceSpec identicalTo: prjct ]
		packageDo: [ :ignored | self assert: false ]
		groupDo: [ :ignored | self assert: false ].
	project := projectReferenceSpec referencedSpec.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'ConfigurationOfProject'.
	self assert: project operator identicalTo: #<.
	self assert: project loads equals: #('MyPackage').
	self assert: project preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoIt.
	project projectDo: [ :prjct | self assert: project identicalTo: prjct ] packageDo: [ :ignored | self assert: false ] groupDo: [ :ignored | self assert: false ]
]

{ #category : 'tests' }
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

{ #category : 'tests' }
MetacelloPackagesSpecTestCase >> testCopyToPackage [
	"not currently used by MetacelloAbstractVersionConstructor"

	| packages package |
	packages := self packagesSpec.
	packages
		add:
			(self packageSpec
				name: 'Package';
				requires: 'AnotherPackage';
				includes: 'IncludedPackage';
				preLoadDoIt: #preLoadDoIt;
				postLoadDoIt: #postLoadDoIt;
				yourself).
	packages
		copy: 'Package'
		to:
			(self packageSpec
				name: 'PackageCopy';
				yourself).
	package := packages packageNamed: 'PackageCopy' ifAbsent: [ self assert: false ].
	self assert: package name equals: 'PackageCopy'.
	self assert: package requires equals: #('AnotherPackage').
	self assert: package includes equals: #('IncludedPackage').
	self assert: package preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: package postLoadDoIt value identicalTo: #postLoadDoIt
]

{ #category : 'tests' }
MetacelloPackagesSpecTestCase >> testCopyToProject [
	"used by MetacelloAbstractVersionConstructor>>project:copyFrom:with:"

	| packages project referenceSpec |
	packages := self packagesSpec.
	packages
		add:
			(self projectSpec
				name: 'Project';
				className: 'ConfigurationOfProjectA';
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
	self assert: project operator identicalTo: #>=.
	self assert: project loads equals: #('MyPackage' 'MyTests').
	self assert: project preLoadDoIt value identicalTo: #preLoadDoItB.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoItB
]

{ #category : 'tests' }
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

{ #category : 'tests' }
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

{ #category : 'tests' }
MetacelloPackagesSpecTestCase >> testMergePackageA [
	"used by MetacelloAbstractVersionConstructor>>package:with:"

	| packages package |
	packages := self packagesSpec.
	packages
		add:
			(self packageSpec
				name: 'Package';
				requires: 'AnotherPackage';
				includes: 'IncludedPackage';
				preLoadDoIt: #preLoadDoIt;
				postLoadDoIt: #postLoadDoIt;
				yourself).
	packages
		merge:
			(self packageSpec
				name: 'Package';
				requires: 'AndAnotherPackage';
				includes: 'AndIncludedPackage';
				yourself).
	package := packages packageNamed: 'Package' ifAbsent: [ self assert: false ].
	self assert: package name equals: 'Package'.
	self assert: package requires equals: #('AnotherPackage' 'AndAnotherPackage').
	self assert: package includes equals: #('IncludedPackage' 'AndIncludedPackage').
	self assert: package preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: package postLoadDoIt value identicalTo: #postLoadDoIt
]

{ #category : 'tests' }
MetacelloPackagesSpecTestCase >> testMergePackageB [
	"used by MetacelloAbstractVersionConstructor>>package:with:"

	| packages package |
	packages := self packagesSpec.
	packages
		add:
			(self packageSpec
				name: 'Package';
				requires: 'AnotherPackage';
				includes: 'IncludedPackage';
				preLoadDoIt: #preLoadDoIt;
				postLoadDoIt: #postLoadDoIt;
				yourself).
	packages
		merge:
			{(self packageSpec
				name: 'Package';
				requires: 'AndAnotherPackage';
				includes: 'AndIncludedPackage';
				yourself)}.
	package := packages packageNamed: 'Package' ifAbsent: [ self assert: false ].
	self assert: package name equals: 'Package'.
	self assert: package requires equals: #('AnotherPackage' 'AndAnotherPackage').
	self assert: package includes equals: #('IncludedPackage' 'AndIncludedPackage').
	self assert: package preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: package postLoadDoIt value identicalTo: #postLoadDoIt
]

{ #category : 'tests' }
MetacelloPackagesSpecTestCase >> testMergePackageD [
    "used by MetacelloAbstractVersionConstructor>>package:with:"

    | packages |
    packages := self packagesSpec.
    packages
        add: 'Platform';
        merge: 'Tests'.
    packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
    packages packageNamed: 'Tests' ifAbsent: [ self assert: false ]
]

{ #category : 'tests' }
MetacelloPackagesSpecTestCase >> testMergeProjectA [
	"used by MetacelloAbstractVersionConstructor>>project:with:"

	| packages project projectReferenceSpec referenceSpec |
	packages := self packagesSpec.
	project := self projectSpec
		name: 'Project';
		className: 'ConfigurationOfProjectA';
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
	self assert: project operator identicalTo: #<.
	self assert: project loads equals: #('MyPackage').
	self assert: project preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoIt.
	project projectDo: [ :prjct | self assert: project identicalTo: prjct ] packageDo: [ :ignored | self assert: false ] groupDo: [ :ignored | self assert: false ]
]

{ #category : 'tests' }
MetacelloPackagesSpecTestCase >> testMergeProjectB [
	"used by MetacelloAbstractVersionConstructor>>project:with:"

	| packages project projectReferenceSpec referenceSpec |
	packages := self packagesSpec.
	project := self projectSpec
		name: 'Project';
		className: 'ConfigurationOfProjectA';
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
	self assert: project operator identicalTo: #<.
	self assert: project loads equals: #('MyPackage').
	self assert: project preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoIt.
	project projectDo: [ :prjct | self assert: project identicalTo: prjct ] packageDo: [ :ignored | self assert: false ] groupDo: [ :ignored | self assert: false ]
]

{ #category : 'tests' }
MetacelloPackagesSpecTestCase >> testPackageMergeSpec [
	| packageA packageB package repository |
	packageA := self packageSpec
		name: 'Package';
		name: 'Package';
		requires: 'AnotherPackage';
		includes: 'IncludedPackage';
		preLoadDoIt: #preLoadDoIt;
		postLoadDoIt: #postLoadDoIt;
		repository: 'http://example.com/repository' username: 'dkh' password: 'password';
		repository: '/opt/gemstone/repository';
		yourself.
	packageB := self packageSpec
		name: 'Package';
		requires: 'AndAnotherPackage';
		includes: 'AndIncludedPackage';
		repository: 'http://example.com/repository' username: 'DaleHenrichs' password: 'secret';
		repository: '/opt/gemstone/repo';
		yourself.
	package := packageA mergeSpec: packageB.
	self assert: package name equals: 'Package'.
	self assert: package requires equals: #('AnotherPackage' 'AndAnotherPackage').
	self assert: package includes equals: #('IncludedPackage' 'AndIncludedPackage').
	self assert: package preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: package postLoadDoIt value identicalTo: #postLoadDoIt.
	repository := package repositories map at: '/opt/gemstone/repository' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'directory'.
	repository := package repositories map at: '/opt/gemstone/repo' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'directory'.
	repository := package repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'http'.
	self assert: repository username equals: 'DaleHenrichs'.
	self assert: repository password equals: 'secret'
]

{ #category : 'tests' }
MetacelloPackagesSpecTestCase >> testPackageSpec [
	| package repository |
	package := self packageSpec
		name: 'Package';
		requires: 'AnotherPackage';
		includes: 'IncludedPackage';
		preLoadDoIt: #preLoadDoIt;
		postLoadDoIt: #postLoadDoIt;
		repository: 'http://example.com/repository' username: 'dkh' password: 'password';
		repository: '/opt/gemstone/repository';
		yourself.
	self assert: package name equals: 'Package'.
	self assert: package requires equals: #('AnotherPackage').
	self assert: package includes equals: #('IncludedPackage').
	self assert: package preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: package postLoadDoIt value identicalTo: #postLoadDoIt.
	repository := package repositories map at: '/opt/gemstone/repository' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'directory'.
	repository := package repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'http'.
	self assert: repository username equals: 'dkh'.
	self assert: repository password equals: 'password'
]

{ #category : 'tests' }
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

{ #category : 'tests' }
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

{ #category : 'tests' }
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

{ #category : 'tests' }
MetacelloPackagesSpecTestCase >> testRemovePackageA [
    "used by MetacelloAbstractVersionConstructor>>removePackage:"

    | packages removed |
    packages := self packagesSpec.
    packages
        add:
            (self packageSpec
                name: 'Package';
                requires: 'AnotherPackage';
                includes: 'IncludedPackage';
                preLoadDoIt: #'preLoadDoIt';
                postLoadDoIt: #'postLoadDoIt';
                yourself).
    packages
        remove:
            (self packageSpec
                name: 'Package';
                yourself).
    removed := false.
    packages packageNamed: 'Package' ifAbsent: [ removed := true ].
    self assert: removed
]

{ #category : 'tests' }
MetacelloPackagesSpecTestCase >> testRemovePackageB [
    "used by MetacelloAbstractVersionConstructor>>removePackage:"

    | packages removed |
    packages := self packagesSpec.
    packages
        add:
            (self packageSpec
                name: 'Package';
                requires: 'AnotherPackage';
                includes: 'IncludedPackage';
                preLoadDoIt: #'preLoadDoIt';
                postLoadDoIt: #'postLoadDoIt';
                yourself).
    packages
        remove:
            {(self packageSpec
                name: 'Package';
                yourself)}.
    removed := false.
    packages packageNamed: 'Package' ifAbsent: [ removed := true ].
    self assert: removed
]

{ #category : 'tests' }
MetacelloPackagesSpecTestCase >> testRemovePackageC [
    "used by MetacelloAbstractVersionConstructor>>removePackage:"

    | packages removed |
    packages := self packagesSpec.
    packages
        add:
            (self packageSpec
                name: 'Package';
                requires: 'AnotherPackage';
                includes: 'IncludedPackage';
                preLoadDoIt: #'preLoadDoIt';
                postLoadDoIt: #'postLoadDoIt';
                yourself).
    packages remove: {'Package'}.
    removed := false.
    packages packageNamed: 'Package' ifAbsent: [ removed := true ].
    self assert: removed
]

{ #category : 'tests' }
MetacelloPackagesSpecTestCase >> testRemovePackageD [
    "used by MetacelloAbstractVersionConstructor>>removePackage:"

    | packages removed |
    packages := self packagesSpec.
    packages
        add:
            (self packageSpec
                name: 'Package';
                requires: 'AnotherPackage';
                includes: 'IncludedPackage';
                preLoadDoIt: #'preLoadDoIt';
                postLoadDoIt: #'postLoadDoIt';
                yourself).
    packages remove: 'Package'.
    removed := false.
    packages packageNamed: 'Package' ifAbsent: [ removed := true ].
    self assert: removed
]

{ #category : 'tests' }
MetacelloPackagesSpecTestCase >> testRemoveProjectA [
	"used by MetacelloAbstractVersionConstructor>>removeProject:"

	| packages project projectReferenceSpec removed |
	packages := self packagesSpec.
	packages
		add:
			(self projectSpec
				name: 'Project';
				className: 'ConfigurationOfProject';
				operator: #<;
				loads: #('MyPackage');
				preLoadDoIt: #preLoadDoIt;
				postLoadDoIt: #postLoadDoIt;
				yourself).
	projectReferenceSpec := packages packageNamed: 'Project' ifAbsent: [ self assert: false ].
	project := projectReferenceSpec referencedSpec.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'ConfigurationOfProject'.
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

{ #category : 'tests' }
MetacelloPackagesSpecTestCase >> testRemoveProjectB [
	"used by MetacelloAbstractVersionConstructor>>removeProject:"

	| packages project projectReferenceSpec removed |
	packages := self packagesSpec.
	packages
		add:
			(self projectSpec
				name: 'Project';
				className: 'ConfigurationOfProject';
				operator: #<;
				loads: #('MyPackage');
				preLoadDoIt: #preLoadDoIt;
				postLoadDoIt: #postLoadDoIt;
				yourself).
	projectReferenceSpec := packages packageNamed: 'Project' ifAbsent: [ self assert: false ].
	project := projectReferenceSpec referencedSpec.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'ConfigurationOfProject'.
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
