Class {
	#name : #MetacelloPackageSpecTestCase,
	#superclass : #MetacelloSpecTestCase,
	#category : #'Metacello-TestsMCCore'
}

{ #category : #accessing }
MetacelloPackageSpecTestCase >> projectClass [
    ^ MetacelloMCProject
]

{ #category : #tests }
MetacelloPackageSpecTestCase >> testPackageMergeSpec [
	| packageA packageB package repository |
	packageA := self packageSpec
		name: 'Package';
		name: 'Package';
		requires: 'AnotherPackage';
		includes: 'IncludedPackage';
		answers: #(#('preload' 'preload answer') #('postload' 'postload answer'));
		file: 'Package-dkh.1';
		preLoadDoIt: #preLoadDoIt;
		postLoadDoIt: #postLoadDoIt;
		repository: 'http://example.com/repository' username: 'dkh' password: 'password';
		repository: '/opt/gemstone/repository';
		yourself.
	packageB := self packageSpec
		name: 'Package';
		requires: 'AndAnotherPackage';
		includes: 'AndIncludedPackage';
		answers: #(#('xpostload' 'xpostload answer'));
		file: 'Package-dkh.2';
		repository: 'http://example.com/repository' username: 'DaleHenrichs' password: 'secret';
		repository: '/opt/gemstone/repo';
		yourself.
	package := packageA mergeSpec: packageB.
	self assert: package name equals: 'Package'.
	self assert: package requires equals: #('AnotherPackage' 'AndAnotherPackage').
	self assert: package includes equals: #('IncludedPackage' 'AndIncludedPackage').
	self assert: package answers equals: #(#('preload' 'preload answer') #('postload' 'postload answer') #('xpostload' 'xpostload answer')).
	self assert: package file equals: 'Package-dkh.2'.
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

{ #category : #tests }
MetacelloPackageSpecTestCase >> testPackageSpec [
	| package repository |
	package := self packageSpec
		name: 'Package';
		requires: 'AnotherPackage';
		includes: 'IncludedPackage';
		answers: #(#('preload' 'preload answer') #('postload' 'postload answer'));
		file: 'Package-dkh.1';
		preLoadDoIt: #preLoadDoIt;
		postLoadDoIt: #postLoadDoIt;
		repository: 'http://example.com/repository' username: 'dkh' password: 'password';
		repository: '/opt/gemstone/repository';
		yourself.
	self assert: package name equals: 'Package'.
	self assert: package requires equals: #('AnotherPackage').
	self assert: package includes equals: #('IncludedPackage').
	self assert: package answers equals: #(#('preload' 'preload answer') #('postload' 'postload answer')).
	self assert: package file equals: 'Package-dkh.1'.
	self assert: package preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: package postLoadDoIt value identicalTo: #postLoadDoIt.
	repository := package repositories map at: '/opt/gemstone/repository' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'directory'.
	repository := package repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'http'.
	self assert: repository username equals: 'dkh'.
	self assert: repository password equals: 'password'
]
