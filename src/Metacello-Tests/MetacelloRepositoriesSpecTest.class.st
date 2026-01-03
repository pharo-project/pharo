Class {
	#name : 'MetacelloRepositoriesSpecTest',
	#superclass : 'MetacelloAbstractSpecTest',
	#category : 'Metacello-Tests-Specs',
	#package : 'Metacello-Tests',
	#tag : 'Specs'
}

{ #category : 'tests' }
MetacelloRepositoriesSpecTest >> testAddA [

	| repositories repository |
	repositories := MetacelloRepositoriesSpec new.
	repositories
		add: (MetacelloRepositorySpec new
				 description: 'http://example.com/repository';
				 username: 'dkh';
				 password: 'password';
				 yourself);
		add: (MetacelloRepositorySpec new
				 description: 'http://example.com/repository';
				 username: 'DaleHenrichs';
				 password: 'secret';
				 yourself).
	repository := repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
	self assert: repository description equals: 'http://example.com/repository'.
	self assert: repository type equals: 'http'.
	self assert: repository username equals: 'DaleHenrichs'.
	self assert: repository password equals: 'secret'
]

{ #category : 'tests' }
MetacelloRepositoriesSpecTest >> testAddB [

	| repositories repository |
	repositories := MetacelloRepositoriesSpec new.
	repositories
		add: (MetacelloRepositorySpec new
				 description: 'http://example.com/repository';
				 username: 'dkh';
				 password: 'password';
				 yourself);
		add: '/opt/gemstone/repository'.
	repository := repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
	self assert: repository description equals: 'http://example.com/repository'.
	self assert: repository type equals: 'http'.
	self assert: repository username equals: 'dkh'.
	self assert: repository password equals: 'password'.
	repository := repositories map at: '/opt/gemstone/repository' ifAbsent: [ self assert: false ].
	self assert: repository description equals: '/opt/gemstone/repository'.
	self assert: repository type equals: 'directory'
]

{ #category : 'tests' }
MetacelloRepositoriesSpecTest >> testAddC [

	| repositories repository |
	repositories := MetacelloRepositoriesSpec new.
	repositories add: #( 'http://example.com/repository' '/opt/gemstone/repository' ).
	repository := repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
	self assert: repository description equals: 'http://example.com/repository'.
	self assert: repository type equals: 'http'.
	repository := repositories map at: '/opt/gemstone/repository' ifAbsent: [ self assert: false ].
	self assert: repository description equals: '/opt/gemstone/repository'.
	self assert: repository type equals: 'directory'
]

{ #category : 'tests' }
MetacelloRepositoriesSpecTest >> testAddE [

	| repositories repository |
	repositories := MetacelloRepositoriesSpec new.
	repositories
		repository: 'http://example.com/repository' username: 'dkh' password: 'password';
		repository: '/opt/gemstone/repository'.
	repository := repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
	self assert: repository description equals: 'http://example.com/repository'.
	self assert: repository type equals: 'http'.
	repository := repositories map at: '/opt/gemstone/repository' ifAbsent: [ self assert: false ].
	self assert: repository description equals: '/opt/gemstone/repository'.
	self assert: repository type equals: 'directory'
]

{ #category : 'tests' }
MetacelloRepositoriesSpecTest >> testAddF [

	| repositories repository |
	repositories := MetacelloRepositoriesSpec new.
	repositories
		repository: (MetacelloRepositorySpec new
				 description: 'http://example.com/repository';
				 username: 'dkh';
				 password: 'password';
				 yourself);
		repository: (MetacelloRepositorySpec new
				 description: 'http://example.com/repository';
				 username: 'DaleHenrichs';
				 password: 'secret';
				 yourself).
	repository := repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
	self assert: repository description equals: 'http://example.com/repository'.
	self assert: repository type equals: 'http'.
	self assert: repository username equals: 'DaleHenrichs'.
	self assert: repository password equals: 'secret'
]

{ #category : 'tests' }
MetacelloRepositoriesSpecTest >> testCopyTo [

	| repositories |
	repositories := MetacelloRepositoriesSpec new.
	repositories add: (MetacelloRepositorySpec new
			 description: 'http://example.com/repository';
			 username: 'dkh';
			 password: 'password';
			 yourself).
	self
		should: [
				repositories copy: 'http://example.com/repository' to: (MetacelloRepositorySpec new
						 description: 'http://example.com/alternate/repository';
						 yourself) ]
		raise: Error
]

{ #category : 'tests' }
MetacelloRepositoriesSpecTest >> testMergeA [

	| repositories repository |
	repositories := MetacelloRepositoriesSpec new.
	repositories
		add: (MetacelloRepositorySpec new
				 description: 'http://example.com/repository';
				 username: 'dkh';
				 password: 'password';
				 yourself);
		merge: (MetacelloRepositorySpec new
				 description: 'http://example.com/repository';
				 username: 'DaleHenrichs';
				 password: 'secret';
				 yourself).
	repository := repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
	self assert: repository description equals: 'http://example.com/repository'.
	self assert: repository type equals: 'http'.
	self assert: repository username equals: 'DaleHenrichs'.
	self assert: repository password equals: 'secret'
]

{ #category : 'tests' }
MetacelloRepositoriesSpecTest >> testMergeB [

	| repositories repository |
	repositories := MetacelloRepositoriesSpec new.
	repositories
		add: (MetacelloRepositorySpec new
				 description: 'http://example.com/repository';
				 username: 'dkh';
				 password: 'password';
				 yourself);
		merge: (MetacelloRepositorySpec new
				 description: 'http://example.com/repository';
				 yourself).
	repository := repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
	self assert: repository description equals: 'http://example.com/repository'.
	self assert: repository type equals: 'http'.
	self assert: repository username equals: 'dkh'.
	self assert: repository password equals: 'password'
]

{ #category : 'tests' }
MetacelloRepositoriesSpecTest >> testMergeC [

	| repositories repository |
	repositories := MetacelloRepositoriesSpec new.
	repositories
		add: (MetacelloRepositorySpec new
				 description: 'http://example.com/repository';
				 username: 'dkh';
				 password: 'password';
				 yourself);
		merge: (MetacelloRepositorySpec new
				 description: 'http://example.com/repository';
				 password: 'secret';
				 yourself);
		merge: (MetacelloRepositorySpec new
				 description: 'http://example.com/repository';
				 username: 'DaleHenrichs';
				 yourself).
	repository := repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
	self assert: repository description equals: 'http://example.com/repository'.
	self assert: repository type equals: 'http'.
	self assert: repository username equals: 'DaleHenrichs'.
	self assert: repository password equals: 'secret'
]

{ #category : 'tests' }
MetacelloRepositoriesSpecTest >> testRemoveA [

	| repositories removed |
	repositories := MetacelloRepositoriesSpec new.
	repositories add: (MetacelloRepositorySpec new
			 description: 'http://example.com/repository';
			 username: 'dkh';
			 password: 'password';
			 yourself).
	repositories remove: (MetacelloRepositorySpec new
			 description: 'http://example.com/repository';
			 yourself).
	removed := false.
	repositories map at: 'http://example.com/repository' ifAbsent: [ removed := true ].
	self assert: removed
]

{ #category : 'tests' }
MetacelloRepositoriesSpecTest >> testRemoveB [

	| repositories removed |
	repositories := MetacelloRepositoriesSpec new.
	repositories add: (MetacelloRepositorySpec new
			 description: 'http://example.com/repository';
			 username: 'dkh';
			 password: 'password';
			 yourself).
	repositories remove: (MetacelloRepositorySpec new
			 description: 'http://example.com/repository';
			 yourself).
	removed := false.
	repositories map at: 'http://example.com/repository' ifAbsent: [ removed := true ].
	self assert: removed
]

{ #category : 'tests' }
MetacelloRepositoriesSpecTest >> testRemoveC [

	| repositories removed |
	repositories := MetacelloRepositoriesSpec new.
	repositories add: (MetacelloRepositorySpec new
			 description: 'http://example.com/repository';
			 username: 'dkh';
			 password: 'password';
			 yourself).
	repositories remove: (MetacelloRepositorySpec new
			 description: 'http://example.com/repository';
			 yourself).
	removed := false.
	repositories map at: 'http://example.com/repository' ifAbsent: [ removed := true ].
	self assert: removed
]

{ #category : 'tests' }
MetacelloRepositoriesSpecTest >> testRemoveD [

	| repositories removed |
	repositories := MetacelloRepositoriesSpec new.
	repositories add: (MetacelloRepositorySpec new
			 description: 'http://example.com/repository';
			 username: 'dkh';
			 password: 'password';
			 yourself).
	repositories remove: (MetacelloRepositorySpec new
			 description: 'http://example.com/repository';
			 yourself).
	removed := false.
	repositories map at: 'http://example.com/repository' ifAbsent: [ removed := true ].
	self assert: removed
]
