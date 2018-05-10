"
SUnit tests for Gofer API
"
Class {
	#name : #GoferApiTest,
	#superclass : #GoferTest,
	#category : #'Gofer-Tests-Tests'
}

{ #category : #asserting }
GoferApiTest >> assert: registry on: serverUrlString hasUser: nameString withPassword: passwordString [ 

	registry repositoryAt: serverUrlString credentialsDo: [ :user :password |
		self assert: nameString equals: user.
		self assert: passwordString equals: password ].
]

{ #category : #utilities }
GoferApiTest >> assert: aGofer repositories: anArray [
	self assert: aGofer repositories size equals: anArray size.
	aGofer repositories with: anArray
		do: [ :first :second | 
			self assert: first description equals: second ]
]

{ #category : #'tests - repositories - places' }
GoferApiTest >> testBlueplane [
	gofer blueplane: 'SIXX'.
	self assert: gofer repositories: #('http://squeaksource.blueplane.jp/SIXX')
]

{ #category : #'tests - references' }
GoferApiTest >> testConstraintReference [
	|reference|
	gofer 
		repository: self monticelloRepository; 
		package: 'GoferBar' constraint: [ :ref | ref branch = 'branch' ].
	
	self assert: gofer resolved size equals: 1.
	
	reference := gofer resolved first.
	
	self assert: (reference isKindOf: GoferResolvedReference).
	self assert: reference packageName equals: 'GoferBar'.
	self assert: reference author equals: 'lr'.
	self assert: reference branch equals: 'branch'.
	self assert: reference versionNumber equals: 2.
	self assert: reference repository equals: self monticelloRepository
]

{ #category : #'tests - repositories' }
GoferApiTest >> testCustomRepository [

	gofer repository: self monticelloRepository.
	
	self assert: gofer repositories: (Array with: self monticelloRepository description).
	self assert: gofer repositories first equals: self monticelloRepository
]

{ #category : #'tests - repositories' }
GoferApiTest >> testDirectoryRepository [
	gofer directory: FileSystem disk workingDirectory fullName.

	self assert: gofer repositories: (Array with: FileSystem disk workingDirectory fullName).
	self assert: (gofer repositories first isKindOf: MCDirectoryRepository)
]

{ #category : #'tests - repositories - places' }
GoferApiTest >> testGemsource [
	gofer gemsource: 'Seaside29'.
	self assert: gofer repositories: #('http://seaside.gemstone.com/ss/Seaside29')
]

{ #category : #'tests - repositories' }
GoferApiTest >> testHttpRepository [

	gofer url: 'http://source.lukas-renggli.ch/pier' username: 'foo' password: 'bar'.

	self assert: gofer repositories: #('http://source.lukas-renggli.ch/pier').
	self assert: (gofer repositories first isKindOf: MCHttpRepository)
]

{ #category : #'tests - repositories' }
GoferApiTest >> testHttpRepositoryOverrideStoredCredentials [

	| exampleServer registry |
	[
		exampleServer := 'http://my.cool.repo.server.com/'.
		registry := MCServerRegistry uniqueInstance.
		registry on: exampleServer beUser: 'myusername' withPassword: 'mypassword'.
		
		gofer url: exampleServer username: 'foo' password: 'bar'.
		
		self assert: gofer repositories: { exampleServer }.
		self assert: gofer repositories first user equals: 'foo'.
		self assert: registry on: exampleServer hasUser: 'myusername' withPassword: 'mypassword'.
	] ensure: [
		registry removeCredentialsFor: exampleServer ].
	
]

{ #category : #'tests - repositories' }
GoferApiTest >> testHttpRepositoryUseStoredCredentials [

	| exampleServer registry |
	[
		exampleServer := 'http://my.cool.repo.server.com/'.
		registry := MCServerRegistry uniqueInstance.
		registry on: exampleServer beUser: 'myusername' withPassword: 'mypassword'.
		
		gofer url: exampleServer.
		
		self assert: gofer repositories: { exampleServer }.
		self assert: gofer repositories first user equals: 'myusername'
	] ensure: [
		registry removeCredentialsFor: exampleServer ].
	
]

{ #category : #'tests - repositories - places' }
GoferApiTest >> testImpara [
	gofer impara: 'Tweak'.
	self assert: gofer repositories: #('http://source.impara.de/Tweak')
]

{ #category : #tests }
GoferApiTest >> testInitialReferences [
	self assert: gofer references isEmpty
]

{ #category : #tests }
GoferApiTest >> testInitialRepositories [

	gofer := Gofer new.

	self assert: gofer repositories size equals: 1.
	self assert: (gofer repositories first isKindOf: MCCacheRepository uniqueInstance class)
]

{ #category : #'tests - repositories - options' }
GoferApiTest >> testPackageCache [
	gofer squeaksource: 'r1'; squeaksource: 'r2'.
	
	gofer enablePackageCache.
	self assert: gofer repositories: (Array with: MCCacheRepository uniqueInstance description) , #('http://www.squeaksource.com/r1' 'http://www.squeaksource.com/r2').
	
	gofer disablePackageCache.
	self assert: gofer repositories: #('http://www.squeaksource.com/r1' 'http://www.squeaksource.com/r2')
]

{ #category : #'tests - references' }
GoferApiTest >> testPackageReference [
	|reference|
	gofer 
		repository: self monticelloRepository;
		package: 'GoferFoo'.

	self assert: gofer resolved size equals: 1.

	reference := gofer resolved first.
	
	self assert: (reference isKindOf: GoferResolvedReference).
	self assert: reference packageName equals: 'GoferFoo'.
	self assert: reference author equals: 'lr'.
	self assert: reference branch isEmpty.
	self assert: reference versionNumber equals: 4.
	self assert: reference repository equals: self monticelloRepository
]

{ #category : #'tests - repositories - places' }
GoferApiTest >> testRenggli [
	gofer renggli: 'pier'.
	self assert: gofer repositories: #('http://source.lukas-renggli.ch/pier')
]

{ #category : #'tests - repositories - options' }
GoferApiTest >> testRepositoryErrors [
	gofer
		url: 'http://pharo-project.org/page-that-will-never-ever-exist';
		repository: self monticelloRepository.
	gofer package: 'GoferFoo'.
	gofer enableRepositoryErrors.
	self should: [ gofer resolved ] raise: GoferRepositoryError.
	gofer disableRepositoryErrors.
	gofer resolved
]

{ #category : #'tests - repositories - places' }
GoferApiTest >> testSmalltalkhub [
	gofer smalltalkhubUser: 'dh83' project: 'ci'.
	self assert: gofer repositories: #('http://smalltalkhub.com/mc/dh83/ci/main/')
]

{ #category : #'tests - repositories - places' }
GoferApiTest >> testSqueakfoundation [
	gofer squeakfoundation: '39a'.
	self assert: gofer repositories: #('http://source.squeakfoundation.org/39a')
]

{ #category : #'tests - repositories - places' }
GoferApiTest >> testSqueaksource [
	gofer squeaksource: 'Seaside29'.
	self assert: gofer repositories: #('http://www.squeaksource.com/Seaside29')
]

{ #category : #'tests - repositories' }
GoferApiTest >> testSubDirectoryRepository [
	Smalltalk globals
		at: #MCSubDirectoryRepository
		ifPresent: [ :subDirectoryRepositoryClass | |path|
			path := (FileSystem disk workingDirectory / '*') fullName.
			gofer directory: path.
			self assert: gofer repositories: (Array with: path).
			self assert: (gofer repositories first isKindOf: subDirectoryRepositoryClass) ]
]

{ #category : #'tests - references' }
GoferApiTest >> testVersionReference [
	|reference|
	gofer 
		repository: self monticelloRepository; 
		version: 'GoferFoo-lr.2'.
	self assert: gofer resolved size equals: 1.
	
	reference := gofer resolved first.
	
	self assert: (reference isKindOf: GoferResolvedReference).
	self assert: reference packageName equals: 'GoferFoo'.
	self assert: reference author equals: 'lr'.
	self assert: reference branch isEmpty.
	self assert: reference versionNumber equals: 2.
	self assert: reference repository equals: self monticelloRepository
]

{ #category : #'tests - repositories - places' }
GoferApiTest >> testWiresong [
	gofer wiresong: 'ob'.
	self assert: gofer repositories: #('http://source.wiresong.ca/ob')
]
