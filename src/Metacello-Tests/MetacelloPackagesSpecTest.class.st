Class {
	#name : 'MetacelloPackagesSpecTest',
	#superclass : 'MetacelloAbstractSpecTest',
	#category : 'Metacello-Tests-Specs',
	#package : 'Metacello-Tests',
	#tag : 'Specs'
}

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testAddGroupA [
	"used by MetacelloBaselineConstructor>>group:overrides:"

	| packages group |
	packages := MetacelloPackagesSpec new.
	packages add: (self groupSpec
			 name: 'Platform';
			 includes: 'Core';
			 yourself).
	packages add: (self groupSpec
			 name: 'Platform';
			 includes: 'Tests';
			 yourself).
	group := packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
	self deny: (group includes includes: 'Core').
	self assert: (group includes includes: 'Tests')
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testAddGroupB [
	"used by MetacelloBaselineConstructor>>group:overrides:"

	| packages group |
	packages := MetacelloPackagesSpec new.
	packages add: (self groupSpec
			 name: 'Platform';
			 includes: 'Core';
			 yourself).
	packages add: (self groupSpec
			 name: 'Platform';
			 includes: 'Tests';
			 yourself).
	group := packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
	self deny: (group includes includes: 'Core').
	self assert: (group includes includes: 'Tests')
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testAddPackageA [
	"used by MetacelloBaselineConstructor>>package: and MetacelloBaselineConstructor>>package:overrides:"

	| packages package |
	packages := MetacelloPackagesSpec new.
	packages add: (self packageSpec
			 name: 'Package';
			 requires: 'AnotherPackage';
			 includes: 'IncludedPackage';
			 preLoadDoIt: #preLoadDoIt;
			 postLoadDoIt: #postLoadDoIt;
			 yourself).
	packages add: (self packageSpec
			 name: 'Package';
			 requires: 'AndAnotherPackage';
			 includes: 'AndIncludedPackage';
			 yourself).
	package := packages packageNamed: 'Package' ifAbsent: [ self assert: false ].
	self assert: package name equals: 'Package'.
	self assert: package requires equals: #( 'AndAnotherPackage' ).
	self assert: package includes equals: #( 'AndIncludedPackage' ).
	self assert: package preLoadDoIt value identicalTo: nil.
	self assert: package postLoadDoIt value identicalTo: nil
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testAddPackageB [
	"used by MetacelloBaselineConstructor>>package: and MetacelloBaselineConstructor>>package:overrides:"

	| packages package |
	packages := MetacelloPackagesSpec new.
	packages add: (self packageSpec
			 name: 'Package';
			 requires: 'AnotherPackage';
			 includes: 'IncludedPackage';
			 preLoadDoIt: #preLoadDoIt;
			 postLoadDoIt: #postLoadDoIt;
			 yourself).
	packages add: (self packageSpec
			 name: 'Package';
			 requires: 'AndAnotherPackage';
			 includes: 'AndIncludedPackage';
			 yourself).
	package := packages packageNamed: 'Package' ifAbsent: [ self assert: false ].
	self assert: package name equals: 'Package'.
	self assert: package requires equals: #( 'AndAnotherPackage' ).
	self assert: package includes equals: #( 'AndIncludedPackage' ).
	self assert: package preLoadDoIt value identicalTo: nil.
	self assert: package postLoadDoIt value identicalTo: nil
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testAddPackageC [
	"used by MetacelloBaselineConstructor>>package: and MetacelloBaselineConstructor>>package:overrides:"

	| packages |
	packages := MetacelloPackagesSpec new.
	packages add: (self packageSpec
			 name: 'Platform';
			 yourself).
	packages add: (self packageSpec
			 name: 'Base';
			 yourself).
	packages add: (self packageSpec
			 name: 'Tests';
			 yourself).
	packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
	packages packageNamed: 'Base' ifAbsent: [ self assert: false ].
	packages packageNamed: 'Tests' ifAbsent: [ self assert: false ]
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testAddPackageD [
	"used by MetacelloBaselineConstructor>>package: and MetacelloBaselineConstructor>>package:overrides:"

	| packages |
	packages := MetacelloPackagesSpec new.
	packages add: (self packageSpec
			 name: 'Platform';
			 yourself).
	packages packageNamed: 'Platform' ifAbsent: [ self assert: false ]
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testAddProjectA [
	"used by MetacelloBaselineConstructor>>project:overrides:"

	| packages project projectReferenceSpec |
	packages := MetacelloPackagesSpec new.
	packages add: (self projectSpec
			 name: 'Project';
			 className: 'BaselineOfProjectA';
			 loads: #( 'MyPackage' 'MyTests' );
			 preLoadDoIt: #preLoadDoItB;
			 postLoadDoIt: #postLoadDoItB;
			 yourself).
	packages add: (self projectSpec
			 name: 'Project';
			 className: 'BaselineOfProject';
			 loads: #( 'MyPackage' );
			 preLoadDoIt: #preLoadDoIt;
			 postLoadDoIt: #postLoadDoIt;
			 yourself).
	projectReferenceSpec := packages packageNamed: 'Project' ifAbsent: [ self assert: false ].
	self assert: projectReferenceSpec projectName equals: 'Project'.
	self should: [ projectReferenceSpec includes: #(  ) ] raise: Error.
	self should: [ projectReferenceSpec requires: #(  ) ] raise: Error.
	projectReferenceSpec
		projectDo: [ :prjct | self assert: projectReferenceSpec identicalTo: prjct ]
		packageDo: [ :ignored | self assert: false ]
		groupDo: [ :ignored | self assert: false ].
	project := projectReferenceSpec referencedSpec.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'BaselineOfProject'.
	self assert: project loads equals: #( 'MyPackage' ).
	self assert: project preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoIt.
	project projectDo: [ :prjct | self assert: project identicalTo: prjct ] packageDo: [ :ignored | self assert: false ] groupDo: [ :ignored | self assert: false ]
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testAddProjectB [
	"used by MetacelloBaselineConstructor>>project:overrides:"

	| packages project projectReferenceSpec |
	packages := MetacelloPackagesSpec new.
	packages add: (self projectSpec
			 name: 'Project';
			 className: 'BaselineOfProjectA';
			 loads: #( 'MyPackage' 'MyTests' );
			 preLoadDoIt: #preLoadDoItB;
			 postLoadDoIt: #postLoadDoItB;
			 yourself).
	packages add: (self projectSpec
			 name: 'Project';
			 className: 'BaselineOfProject';
			 loads: #( 'MyPackage' );
			 preLoadDoIt: #preLoadDoIt;
			 postLoadDoIt: #postLoadDoIt;
			 yourself).
	projectReferenceSpec := packages packageNamed: 'Project' ifAbsent: [ self assert: false ].
	self assert: projectReferenceSpec projectName equals: 'Project'.
	self should: [ projectReferenceSpec includes: #(  ) ] raise: Error.
	self should: [ projectReferenceSpec requires: #(  ) ] raise: Error.
	projectReferenceSpec
		projectDo: [ :prjct | self assert: projectReferenceSpec identicalTo: prjct ]
		packageDo: [ :ignored | self assert: false ]
		groupDo: [ :ignored | self assert: false ].
	project := projectReferenceSpec referencedSpec.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'BaselineOfProject'.
	self assert: project loads equals: #( 'MyPackage' ).
	self assert: project preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoIt.
	project projectDo: [ :prjct | self assert: project identicalTo: prjct ] packageDo: [ :ignored | self assert: false ] groupDo: [ :ignored | self assert: false ]
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testCopyToGroup [
	"not currently used by MetacelloBaselineConstructor"

	| packages group |
	packages := MetacelloPackagesSpec new.
	packages add: (self groupSpec
			 name: 'Platform';
			 includes: 'Core';
			 yourself).
	packages merge: (self groupSpec
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
MetacelloPackagesSpecTest >> testCopyToPackage [
	"not currently used by MetacelloBaselineConstructor"

	| packages package |
	packages := MetacelloPackagesSpec new.
	packages add: (self packageSpec
			 name: 'Package';
			 requires: 'AnotherPackage';
			 includes: 'IncludedPackage';
			 preLoadDoIt: #preLoadDoIt;
			 postLoadDoIt: #postLoadDoIt;
			 yourself).
	packages copy: 'Package' to: (self packageSpec
			 name: 'PackageCopy';
			 yourself).
	package := packages packageNamed: 'PackageCopy' ifAbsent: [ self assert: false ].
	self assert: package name equals: 'PackageCopy'.
	self assert: package requires equals: #( 'AnotherPackage' ).
	self assert: package includes equals: #( 'IncludedPackage' ).
	self assert: package preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: package postLoadDoIt value identicalTo: #postLoadDoIt
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testCopyToProject [
	"used by MetacelloBaselineConstructor>>project:copyFrom:with:"

	| packages project referenceSpec |
	packages := MetacelloPackagesSpec new.
	packages add: (self projectSpec
			 name: 'Project';
			 className: 'BaselineOfProjectA';
			 loads: #( 'MyPackage' 'MyTests' );
			 preLoadDoIt: #preLoadDoItB;
			 postLoadDoIt: #postLoadDoItB;
			 yourself).
	project := self projectSpec
		           name: 'ProjectCopy';
		           yourself.
	referenceSpec := MetacelloProjectReferenceSpec projectReference: project.
	packages copy: 'Project' to: referenceSpec.
	project := (packages packageNamed: 'ProjectCopy' ifAbsent: [ self assert: false ]) referencedSpec.
	self assert: project name equals: 'ProjectCopy'.
	self assert: project className equals: 'BaselineOfProjectA'.
	self assert: project loads equals: #( 'MyPackage' 'MyTests' ).
	self assert: project preLoadDoIt value identicalTo: #preLoadDoItB.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoItB
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testMergeGroupA [
	"used by MetacelloBaselineConstructor>>group:with:"

	| packages group |
	packages := MetacelloPackagesSpec new.
	packages add: (self groupSpec
			 name: 'Platform';
			 includes: 'Core';
			 yourself).
	packages merge: (self groupSpec
			 name: 'Platform';
			 includes: 'Tests';
			 yourself).
	group := packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
	self assert: (group includes includes: 'Core').
	self assert: (group includes includes: 'Tests')
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testMergeGroupB [
	"used by MetacelloBaselineConstructor>>group:with:"

	| packages group |
	packages := MetacelloPackagesSpec new.
	packages add: (self groupSpec
			 name: 'Platform';
			 includes: 'Core';
			 yourself).
	packages merge: (self groupSpec
			 name: 'Platform';
			 includes: 'Tests';
			 yourself).
	group := packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
	self assert: (group includes includes: 'Core').
	self assert: (group includes includes: 'Tests')
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testMergePackageA [
	"used by MetacelloBaselineConstructor>>package:with:"

	| packages package |
	packages := MetacelloPackagesSpec new.
	packages add: (self packageSpec
			 name: 'Package';
			 requires: 'AnotherPackage';
			 includes: 'IncludedPackage';
			 preLoadDoIt: #preLoadDoIt;
			 postLoadDoIt: #postLoadDoIt;
			 yourself).
	packages merge: (self packageSpec
			 name: 'Package';
			 requires: 'AndAnotherPackage';
			 includes: 'AndIncludedPackage';
			 yourself).
	package := packages packageNamed: 'Package' ifAbsent: [ self assert: false ].
	self assert: package name equals: 'Package'.
	self assert: package requires equals: #( 'AnotherPackage' 'AndAnotherPackage' ).
	self assert: package includes equals: #( 'IncludedPackage' 'AndIncludedPackage' ).
	self assert: package preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: package postLoadDoIt value identicalTo: #postLoadDoIt
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testMergePackageB [
	"used by MetacelloBaselineConstructor>>package:with:"

	| packages package |
	packages := MetacelloPackagesSpec new.
	packages add: (self packageSpec
			 name: 'Package';
			 requires: 'AnotherPackage';
			 includes: 'IncludedPackage';
			 preLoadDoIt: #preLoadDoIt;
			 postLoadDoIt: #postLoadDoIt;
			 yourself).
	packages merge: (self packageSpec
			 name: 'Package';
			 requires: 'AndAnotherPackage';
			 includes: 'AndIncludedPackage';
			 yourself).
	package := packages packageNamed: 'Package' ifAbsent: [ self assert: false ].
	self assert: package name equals: 'Package'.
	self assert: package requires equals: #( 'AnotherPackage' 'AndAnotherPackage' ).
	self assert: package includes equals: #( 'IncludedPackage' 'AndIncludedPackage' ).
	self assert: package preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: package postLoadDoIt value identicalTo: #postLoadDoIt
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testMergePackageD [
	"used by MetacelloBaselineConstructor>>package:with:"

	| packages |
	packages := MetacelloPackagesSpec new.
	packages
		add: (self packageSpec
				 name: 'Platform';
				 yourself);
		merge: (self packageSpec
				 name: 'Tests';
				 yourself).
	packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
	packages packageNamed: 'Tests' ifAbsent: [ self assert: false ]
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testMergeProjectA [
	"used by MetacelloBaselineConstructor>>project:with:"

	| packages project projectReferenceSpec referenceSpec |
	packages := MetacelloPackagesSpec new.
	project := self projectSpec
		           name: 'Project';
		           className: 'BaselineOfProjectA';
		           loads: #( 'MyPackage' 'MyTests' );
		           preLoadDoIt: #preLoadDoItB;
		           postLoadDoIt: #postLoadDoItB;
		           yourself.
	referenceSpec := MetacelloProjectReferenceSpec projectReference: project.
	packages add: referenceSpec.
	project := self projectSpec
		           name: 'Project';
		           className: 'BaselineOfProject';
		           loads: #( 'MyPackage' );
		           preLoadDoIt: #preLoadDoIt;
		           postLoadDoIt: #postLoadDoIt;
		           yourself.
	referenceSpec := MetacelloProjectReferenceSpec projectReference: project.
	packages merge: referenceSpec.
	projectReferenceSpec := packages packageNamed: 'Project' ifAbsent: [ self assert: false ].
	project := projectReferenceSpec referencedSpec.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'BaselineOfProject'.
	self assert: project loads equals: #( 'MyPackage' ).
	self assert: project preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoIt.
	project projectDo: [ :prjct | self assert: project identicalTo: prjct ] packageDo: [ :ignored | self assert: false ] groupDo: [ :ignored | self assert: false ]
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testMergeProjectB [
	"used by MetacelloBaselineConstructor>>project:with:"

	| packages project projectReferenceSpec referenceSpec |
	packages := MetacelloPackagesSpec new.
	project := self projectSpec
		           name: 'Project';
		           className: 'BaselineOfProjectA';
		           loads: #( 'MyPackage' 'MyTests' );
		           preLoadDoIt: #preLoadDoItB;
		           postLoadDoIt: #postLoadDoItB;
		           yourself.
	referenceSpec := MetacelloProjectReferenceSpec projectReference: project.
	packages add: referenceSpec.
	project := self projectSpec
		           name: 'Project';
		           className: 'BaselineOfProject';
		           loads: #( 'MyPackage' );
		           preLoadDoIt: #preLoadDoIt;
		           postLoadDoIt: #postLoadDoIt;
		           yourself.
	referenceSpec := MetacelloProjectReferenceSpec projectReference: project.
	packages merge: referenceSpec.
	projectReferenceSpec := packages packageNamed: 'Project' ifAbsent: [ self assert: false ].
	project := projectReferenceSpec referencedSpec.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'BaselineOfProject'.
	self assert: project loads equals: #( 'MyPackage' ).
	self assert: project preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoIt.
	project projectDo: [ :prjct | self assert: project identicalTo: prjct ] packageDo: [ :ignored | self assert: false ] groupDo: [ :ignored | self assert: false ]
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testPackageMergeSpec [
	| packageA packageB package repository |
	packageA := self packageSpec
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
MetacelloPackagesSpecTest >> testPackageSpec [
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
MetacelloPackagesSpecTest >> testRemoveGroupA [
	"used by MetacelloBaselineConstructor>>removeGroup:"

	| packages group removed |
	packages := MetacelloPackagesSpec new.
	packages add: (self groupSpec
			 name: 'Platform';
			 includes: 'Core';
			 yourself).
	packages merge: (self groupSpec
			 name: 'Platform';
			 includes: 'Tests';
			 yourself).
	group := packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
	self assert: (group includes includes: 'Core').
	self assert: (group includes includes: 'Tests').
	packages remove: (self groupSpec
			 name: 'Platform';
			 includes: 'Core';
			 yourself).
	removed := false.
	packages packageNamed: 'Platform' ifAbsent: [ removed := true ].
	self assert: removed
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testRemoveGroupB [
	"used by MetacelloBaselineConstructor>>removeGroup:"

	| packages removed |
	packages := MetacelloPackagesSpec new.
	packages add: (self groupSpec
			 name: 'Platform';
			 includes: 'Core';
			 yourself).
	packages add: (self groupSpec
			 name: 'Base';
			 includes: 'Base';
			 yourself).
	packages add: (self groupSpec
			 name: 'Tests';
			 includes: 'Tests';
			 yourself).
	packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
	packages packageNamed: 'Base' ifAbsent: [ self assert: false ].
	packages packageNamed: 'Tests' ifAbsent: [ self assert: false ].
	packages
		remove: (self groupSpec
				 name: 'Base';
				 yourself);
		remove: (self groupSpec
				 name: 'Tests';
				 yourself).
	packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
	removed := false.
	packages packageNamed: 'Base' ifAbsent: [ removed := true ].
	self assert: removed.
	removed := false.
	packages packageNamed: 'Tests' ifAbsent: [ removed := true ].
	self assert: removed
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testRemoveGroupC [
	"used by MetacelloBaselineConstructor>>removeGroup:"

	| packages removed |
	packages := MetacelloPackagesSpec new.
	packages add: (self groupSpec
			 name: 'Platform';
			 includes: 'Core';
			 yourself).
	packages add: (self groupSpec
			 name: 'Base';
			 includes: 'Base';
			 yourself).
	packages add: (self groupSpec
			 name: 'Tests';
			 includes: 'Tests';
			 yourself).
	packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
	packages packageNamed: 'Base' ifAbsent: [ self assert: false ].
	packages packageNamed: 'Tests' ifAbsent: [ self assert: false ].
	packages remove: (self groupSpec
			 name: 'Tests';
			 yourself).
	packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
	packages packageNamed: 'Base' ifAbsent: [ self assert: false ].
	removed := false.
	packages packageNamed: 'Tests' ifAbsent: [ removed := true ].
	self assert: removed
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testRemovePackageA [
	"used by MetacelloBaselineConstructor>>removePackage:"

	| packages removed |
	packages := MetacelloPackagesSpec new.
	packages add: (self packageSpec
			 name: 'Package';
			 requires: 'AnotherPackage';
			 includes: 'IncludedPackage';
			 preLoadDoIt: #preLoadDoIt;
			 postLoadDoIt: #postLoadDoIt;
			 yourself).
	packages remove: (self packageSpec
			 name: 'Package';
			 yourself).
	removed := false.
	packages packageNamed: 'Package' ifAbsent: [ removed := true ].
	self assert: removed
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testRemovePackageB [
	"used by MetacelloBaselineConstructor>>removePackage:"

	| packages removed |
	packages := MetacelloPackagesSpec new.
	packages add: (self packageSpec
			 name: 'Package';
			 requires: 'AnotherPackage';
			 includes: 'IncludedPackage';
			 preLoadDoIt: #preLoadDoIt;
			 postLoadDoIt: #postLoadDoIt;
			 yourself).
	packages remove: (self packageSpec
			 name: 'Package';
			 yourself).
	removed := false.
	packages packageNamed: 'Package' ifAbsent: [ removed := true ].
	self assert: removed
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testRemovePackageC [
	"used by MetacelloBaselineConstructor>>removePackage:"

	| packages removed |
	packages := MetacelloPackagesSpec new.
	packages add: (self packageSpec
			 name: 'Package';
			 requires: 'AnotherPackage';
			 includes: 'IncludedPackage';
			 preLoadDoIt: #preLoadDoIt;
			 postLoadDoIt: #postLoadDoIt;
			 yourself).
	packages remove: (self packageSpec
			 name: 'Package';
			 yourself).
	removed := false.
	packages packageNamed: 'Package' ifAbsent: [ removed := true ].
	self assert: removed
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testRemovePackageD [
	"used by MetacelloBaselineConstructor>>removePackage:"

	| packages removed |
	packages := MetacelloPackagesSpec new.
	packages add: (self packageSpec
			 name: 'Package';
			 requires: 'AnotherPackage';
			 includes: 'IncludedPackage';
			 preLoadDoIt: #preLoadDoIt;
			 postLoadDoIt: #postLoadDoIt;
			 yourself).
	packages remove: (self packageSpec
			 name: 'Package';
			 yourself).
	removed := false.
	packages packageNamed: 'Package' ifAbsent: [ removed := true ].
	self assert: removed
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testRemoveProjectA [
	"used by MetacelloBaselineConstructor>>removeProject:"

	| packages project projectReferenceSpec removed |
	packages := MetacelloPackagesSpec new.
	packages add: (self projectSpec
			 name: 'Project';
			 className: 'BaselineOfProject';
			 loads: #( 'MyPackage' );
			 preLoadDoIt: #preLoadDoIt;
			 postLoadDoIt: #postLoadDoIt;
			 yourself).
	projectReferenceSpec := packages packageNamed: 'Project' ifAbsent: [ self assert: false ].
	project := projectReferenceSpec referencedSpec.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'BaselineOfProject'.
	self assert: project loads equals: #( 'MyPackage' ).
	self assert: project preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoIt.
	packages remove: (MetacelloProjectReferenceSpec projectReference: project).
	removed := false.
	packages packageNamed: 'Project' ifAbsent: [ removed := true ].
	self assert: removed
]

{ #category : 'tests' }
MetacelloPackagesSpecTest >> testRemoveProjectB [
	"used by MetacelloBaselineConstructor>>removeProject:"

	| packages project projectReferenceSpec removed |
	packages := MetacelloPackagesSpec new.
	packages add: (self projectSpec
			 name: 'Project';
			 className: 'BaselineOfProject';
			 loads: #( 'MyPackage' );
			 preLoadDoIt: #preLoadDoIt;
			 postLoadDoIt: #postLoadDoIt;
			 yourself).
	projectReferenceSpec := packages packageNamed: 'Project' ifAbsent: [ self assert: false ].
	project := projectReferenceSpec referencedSpec.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'BaselineOfProject'.
	self assert: project loads equals: #( 'MyPackage' ).
	self assert: project preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoIt.
	packages remove: (MetacelloProjectReferenceSpec projectReference: project).
	removed := false.
	packages packageNamed: 'Project' ifAbsent: [ removed := true ].
	self assert: removed
]
