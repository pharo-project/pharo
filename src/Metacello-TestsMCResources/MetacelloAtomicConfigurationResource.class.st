Class {
	#name : #MetacelloAtomicConfigurationResource,
	#superclass : #TestResource,
	#instVars : [
		'monticelloRepository'
	],
	#category : #Metacello-TestsMCResources
}

{ #category : #accessing }
MetacelloAtomicConfigurationResource class >> project [

	^self new project
]

{ #category : #ConfigurationOfAtomicFan }
MetacelloAtomicConfigurationResource >> baseline20Fan: spec [ 
	"self reset"
	<version: '2.0-baseline'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec blessing: #baseline.
		spec 
			package: 'GoferFan';
			yourself ].

]

{ #category : #ConfigurationOfAtomicFoo }
MetacelloAtomicConfigurationResource >> baseline20Foo: spec [ 
	"self reset"
	<version: '2.0-baseline'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec blessing: #baseline.
		spec 
			package: 'GoferBar';
			package: 'GoferFoo';
			package: 'GoferBeau';
			package: 'GoferFaux';
			yourself ].

]

{ #category : #ConfigurationOfAtomicFan }
MetacelloAtomicConfigurationResource >> baseline20MethodSourceFan [

	^(self class sourceCodeAt: #baseline20Fan:) asString


]

{ #category : #ConfigurationOfAtomicFoo }
MetacelloAtomicConfigurationResource >> baseline20MethodSourceFoo [

	^(self class sourceCodeAt: #baseline20Foo:) asString


]

{ #category : #ConfigurationOfAtomicFoo }
MetacelloAtomicConfigurationResource >> baseline25Foo: spec [ 
	"self reset"
	<version: '2.5-baseline'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec blessing: #baseline.
		spec 
			package: 'GoferFoo' with: [spec requires: 'GoferFan' ];
			yourself.
		spec 
			project: 'GoferFan' with: [ "requires GoferBeau"
				spec
					className: 'MetacelloTestConfigurationOfAtomicFan';
					versionString: '2.0-baseline';
					loads: 'GoferFan';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself ].

]

{ #category : #ConfigurationOfAtomicFoo }
MetacelloAtomicConfigurationResource >> baseline25MethodSourceFoo [

	^(self class sourceCodeAt: #baseline25Foo:) asString


]

{ #category : #ConfigurationOfAtomicFan }
MetacelloAtomicConfigurationResource >> baseline31Fan: spec [ 
	"self reset"
	<version: '3.1-baseline'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec postLoadDoIt: #postLoad31baseline.
		spec 
			package: 'GoferBar';
			package: 'GoferBeau' with: [ 
				spec 
					requires: 'GoferBar';
					includes: #('GoferFoo' ) ]; 
			package: 'GoferFar' with: [ spec requires: #('GoferFoo' ) ]; 
			yourself.
		spec 
			project: 'GoferFoo' with: [ "requires GoferBeau"
				spec
					className: 'MetacelloTestConfigurationOfLinearFoo';
					versionString: '2.0';
					loads: 'GoferFoo';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself ].

]

{ #category : #ConfigurationOfAtomicFan }
MetacelloAtomicConfigurationResource >> baseline31MethodSourceFan [

	^(self class sourceCodeAt: #baseline31Fan:) asString


]

{ #category : #ConfigurationOfAtomicFoo }
MetacelloAtomicConfigurationResource >> baseline60Foo: spec [ 
	"self reset"
	<version: '6.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			package: 'GoferFoo' with:'GoferFoo-lr.4';
			package: 'GoferBar' with: 'GoferBar-lr.1';
			package: 'GoferFaux' with: 'GoferFaux-tg.34';
			package: 'GoferBeau' with: 'GoferBeau-dkh.55';
			yourself.
		spec
			group: '1' with: #('GoferFoo' 'GoferBar');
			group: '2' with: #('1' 'GoferFaux');
			yourself ].

]

{ #category : #ConfigurationOfAtomicFoo }
MetacelloAtomicConfigurationResource >> baseline60MethodSourceFoo [

	^(self class sourceCodeAt: #baseline60Foo:) asString


]

{ #category : #'Issue 86' }
MetacelloAtomicConfigurationResource >> baseline60MethodSourceProjectIssue86 [

	^(self class sourceCodeAt: #baseline60ProjectIssue86:) asString


]

{ #category : #'Issue 86' }
MetacelloAtomicConfigurationResource >> baseline60ProjectIssue86: spec [ 
	"projects and packages"
	"self reset"
	<version: '6.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			project: 'A' with: [
				spec
					className: 'MetacelloTestConfigurationOfLinearFoo';
					versionString: '6.0';
					loads: #('1');
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			project: 'B' with: [
				spec
					className: 'MetacelloTestConfigurationOfLinearFoo';
					versionString: '6.0';
					loads: #('2');
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself ].

]

{ #category : #ConfigurationOfAtomicFoo }
MetacelloAtomicConfigurationResource >> baseline61Foo: spec [ 
	"self reset"
	<version: '6.1'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			package: 'GoferFoo' with:'GoferFoo-lr.4';
			package: 'GoferBar' with: 'GoferBar-lr.1';
			package: 'GoferFaux' with: 'GoferFaux-tg.34';
			package: 'GoferBeau' with: 'GoferBeau-dkh.55';
			yourself.
		spec
			group: '1' with: #('GoferFoo' 'GoferBar');
			group: '2' with: #('GoferFoo' 'GoferBeau');
			yourself ].

]

{ #category : #ConfigurationOfAtomicFoo }
MetacelloAtomicConfigurationResource >> baseline61MethodSourceFoo [

	^(self class sourceCodeAt: #baseline61Foo:) asString


]

{ #category : #'Issue 86' }
MetacelloAtomicConfigurationResource >> baseline61MethodSourceProjectIssue86 [

	^(self class sourceCodeAt: #baseline61ProjectIssue86:) asString


]

{ #category : #'Issue 86' }
MetacelloAtomicConfigurationResource >> baseline61ProjectIssue86: spec [ 
	"projects and packages"
	"self reset"
	<version: '6.1'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			project: 'A' with: [
				spec
					className: 'MetacelloTestConfigurationOfLinearFoo';
					versionString: '6.1';
					loads: #('1');
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			project: 'B' with: [
				spec
					className: 'MetacelloTestConfigurationOfLinearFoo';
					versionString: '6.1';
					loads: #('2');
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself ].

]

{ #category : #ConfigurationOfAtomicFoo }
MetacelloAtomicConfigurationResource >> baseline62Foo: spec [ 
	"self reset"
	<version: '6.2'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			package: 'GoferFoo' with:'GoferFoo-lr.4';
			package: 'GoferBar' with: 'GoferBar-lr.1';
			package: 'GoferFaux' with: 'GoferFaux-tg.34';
			package: 'GoferBeau' with: 'GoferBeau-dkh.55';
			yourself.
		spec
			group: '1' with: #('GoferFoo' 'GoferBar');
			group: '2' with: #('GoferFoo' );
			yourself ].

]

{ #category : #ConfigurationOfAtomicFoo }
MetacelloAtomicConfigurationResource >> baseline62MethodSourceFoo [

	^(self class sourceCodeAt: #baseline62Foo:) asString


]

{ #category : #'Issue 86' }
MetacelloAtomicConfigurationResource >> baseline62MethodSourceProjectIssue86 [

	^(self class sourceCodeAt: #baseline62ProjectIssue86:) asString


]

{ #category : #'Issue 86' }
MetacelloAtomicConfigurationResource >> baseline62ProjectIssue86: spec [ 
	"projects and packages"
	"self reset"
	<version: '6.2'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			project: 'A' with: [
				spec
					className: 'MetacelloTestConfigurationOfLinearFoo';
					versionString: '6.2';
					loads: #('1');
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			project: 'B' with: [
				spec
					className: 'MetacelloTestConfigurationOfLinearFoo';
					versionString: '6.2';
					loads: #('2');
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself ].

]

{ #category : #ConfigurationOfAtomicFoo }
MetacelloAtomicConfigurationResource >> linearProjectMethodSource [

	^(self class sourceCodeAt: #project) asString copyReplaceAll: 'atomic' with: 'linear'


]

{ #category : #accessing }
MetacelloAtomicConfigurationResource >> monticelloRepository [
	^ monticelloRepository
]

{ #category : #ConfigurationOfAtomicFan }
MetacelloAtomicConfigurationResource >> postLoad31baseline [
	"Throw an error if GoferFar isn't loaded"

	Smalltalk at: #GoferFar ifAbsent: [ self error: 'GoferFar should be loaded' ]
]

{ #category : #ConfigurationOfAtomicFan }
MetacelloAtomicConfigurationResource >> postLoad31baselineMethodSource [

	^(self class sourceCodeAt: #postLoad31baseline) asString


]

{ #category : #ConfigurationOfAtomicFoo }
MetacelloAtomicConfigurationResource >> project [
	"self reset"

	| constructor project |
	"Construct Metacello project"
	constructor := MetacelloVersionConstructor on: self.
	project := constructor project.
	project loader: 
		((project loaderClass new)
			shouldDisablePackageCache: true;
			yourself).
	project loadType: #atomic.
	^project

]

{ #category : #ConfigurationOfFoo }
MetacelloAtomicConfigurationResource >> projectClassMethodSource [

	^(self class class sourceCodeAt: #project) asString


]

{ #category : #ConfigurationOfAtomicFoo }
MetacelloAtomicConfigurationResource >> projectMethodSource [

	^(self class sourceCodeAt: #project) asString


]

{ #category : #running }
MetacelloAtomicConfigurationResource >> setUp [
	super setUp.
	self
		setUpMonticelloRepository;
		setUpConfigurationOfAtomicFoo;
		setUpConfigurationOfLinearFoo;
		setUpConfigurationOfAtomicFan;
		setUpConfigurationOfUmbrellaA;
		setUpConfigurationOfUmbrellaB;
		setUpConfigurationOfProjectIssue86;
		yourself
]

{ #category : #ConfigurationOfAtomicFan }
MetacelloAtomicConfigurationResource >> setUpConfigurationOfAtomicFan [
    "self reset"

    | reference className definitionArray |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfAtomicFan-dkh.1'.
    className := reference packageName asSymbol.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: className)).
    (MCClassDefinition
        name: className
        superclassName: #'Object'
        category: className
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: true
        selector: 'project'
        category: 'cat'
        timeStamp: ''
        source: self projectClassMethodSource).
    (MCMethodDefinition
        className: className asString
        selector: 'project'
        category: 'cat'
        timeStamp: ''
        source: self projectMethodSource).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline20Fan:'
        category: 'cat'
        timeStamp: ''
        source: self baseline20MethodSourceFan).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline31Fan:'
        category: 'cat'
        timeStamp: ''
        source: self baseline31MethodSourceFan).
    (MCMethodDefinition
        className: className asString
        selector: 'postLoad31baseline'
        category: 'cat'
        timeStamp: ''
        source: self postLoad31baselineMethodSource)}.
    monticelloRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #())
]

{ #category : #ConfigurationOfAtomicFoo }
MetacelloAtomicConfigurationResource >> setUpConfigurationOfAtomicFoo [
    "self reset"

    | reference className definitionArray |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfAtomicFoo-dkh.1'.
    className := reference packageName asSymbol.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: className)).
    (MCClassDefinition
        name: className
        superclassName: #'Object'
        category: className
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: true
        selector: 'project'
        category: 'cat'
        timeStamp: ''
        source: self projectClassMethodSource).
    (MCMethodDefinition
        className: className asString
        selector: 'project'
        category: 'cat'
        timeStamp: ''
        source: self projectMethodSource).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline20Foo:'
        category: 'cat'
        timeStamp: ''
        source: self baseline20MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline25Foo:'
        category: 'cat'
        timeStamp: ''
        source: self baseline25MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'version20Foo:'
        category: 'cat'
        timeStamp: ''
        source: self version20MethodSourceFoo)}.
    monticelloRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #())
]

{ #category : #ConfigurationOfAtomicFoo }
MetacelloAtomicConfigurationResource >> setUpConfigurationOfLinearFoo [
    "self reset"

    | reference className definitionArray |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfLinearFoo-dkh.1'.
    className := reference packageName asSymbol.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: className)).
    (MCClassDefinition
        name: className
        superclassName: #'Object'
        category: className
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: true
        selector: 'project'
        category: 'cat'
        timeStamp: ''
        source: self projectClassMethodSource).
    (MCMethodDefinition
        className: className asString
        selector: 'project'
        category: 'cat'
        timeStamp: ''
        source: self linearProjectMethodSource).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline20Foo:'
        category: 'cat'
        timeStamp: ''
        source: self baseline20MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'version20Foo:'
        category: 'cat'
        timeStamp: ''
        source: self version20MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'version50Foo:'
        category: 'cat'
        timeStamp: ''
        source: self version50MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'version51Foo:'
        category: 'cat'
        timeStamp: ''
        source: self version51MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline60Foo:'
        category: 'cat'
        timeStamp: ''
        source: self baseline60MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline61Foo:'
        category: 'cat'
        timeStamp: ''
        source: self baseline61MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline62Foo:'
        category: 'cat'
        timeStamp: ''
        source: self baseline62MethodSourceFoo)}.
    monticelloRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #())
]

{ #category : #'Issue 86' }
MetacelloAtomicConfigurationResource >> setUpConfigurationOfProjectIssue86 [
    "self reset"

    | reference className definitionArray |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectIssue86-dkh.1'.
    className := reference packageName asSymbol.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: className)).
    (MCClassDefinition
        name: className
        superclassName: #'Object'
        category: className
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: true
        selector: 'project'
        category: 'cat'
        timeStamp: ''
        source: self projectClassMethodSource).
    (MCMethodDefinition
        className: className asString
        selector: 'project'
        category: 'cat'
        timeStamp: ''
        source: self linearProjectMethodSource).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline60ProjectIssue86:'
        category: 'cat'
        timeStamp: ''
        source: self baseline60MethodSourceProjectIssue86).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline61ProjectIssue86:'
        category: 'cat'
        timeStamp: ''
        source: self baseline61MethodSourceProjectIssue86).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline62ProjectIssue86:'
        category: 'cat'
        timeStamp: ''
        source: self baseline62MethodSourceProjectIssue86)}.
    monticelloRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #())
]

{ #category : #ConfigurationOfUmbrella }
MetacelloAtomicConfigurationResource >> setUpConfigurationOfUmbrellaA [
    "self reset"

    | reference className definitionArray |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfUmbrellaA-dkh.1'.
    className := reference packageName asSymbol.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: className)).
    (MCClassDefinition
        name: className
        superclassName: #'Object'
        category: className
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: true
        selector: 'project'
        category: 'cat'
        timeStamp: ''
        source: self projectClassMethodSource).
    (MCMethodDefinition
        className: className asString
        selector: 'project'
        category: 'cat'
        timeStamp: ''
        source: self projectMethodSource).
    (MCMethodDefinition
        className: className asString
        selector: 'version50Umbrella:'
        category: 'cat'
        timeStamp: ''
        source: self version50MethodSourceUmbrella)}.
    monticelloRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #())
]

{ #category : #ConfigurationOfUmbrella }
MetacelloAtomicConfigurationResource >> setUpConfigurationOfUmbrellaB [
    "self reset"

    | reference className definitionArray |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfUmbrellaB-dkh.1'.
    className := reference packageName asSymbol.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: className)).
    (MCClassDefinition
        name: className
        superclassName: #'Object'
        category: className
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: true
        selector: 'project'
        category: 'cat'
        timeStamp: ''
        source: self projectClassMethodSource).
    (MCMethodDefinition
        className: className asString
        selector: 'project'
        category: 'cat'
        timeStamp: ''
        source: self projectMethodSource).
    (MCMethodDefinition
        className: className asString
        selector: 'version51Umbrella:'
        category: 'cat'
        timeStamp: ''
        source: self version51MethodSourceUmbrella)}.
    monticelloRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #())
]

{ #category : #running }
MetacelloAtomicConfigurationResource >> setUpMonticelloRepository [
	"This method builds a fake repository with the version references from #buildReferences."
	"self reset"

	monticelloRepository := MCDictionaryRepository new.

]

{ #category : #ConfigurationOfAtomicFoo }
MetacelloAtomicConfigurationResource >> version20Foo: spec [ 
	"self reset"
	<version: '2.0' imports: #('2.0-baseline')>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec blessing: #release.
		spec 
			package: 'GoferBar' with: 'GoferBar-dkh.1';
			package: 'GoferFoo' with: 'GoferFoo-dkh.4';
			package: 'GoferBeau' with: 'GoferBeau-dkh.15';
			package: 'GoferFaux' with: 'GoferFaux-tg.30';
			yourself ].

]

{ #category : #ConfigurationOfAtomicFoo }
MetacelloAtomicConfigurationResource >> version20MethodSourceFoo [

	^(self class sourceCodeAt: #version20Foo:) asString


]

{ #category : #ConfigurationOfAtomicFoo }
MetacelloAtomicConfigurationResource >> version50Foo: spec [ 
	"self reset"
	<version: '5.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec blessing: #release.
		spec 
			package: 'GoferUmbrella' with: 'GoferUmbrella-dkh.4';
			yourself ].

]

{ #category : #ConfigurationOfAtomicFoo }
MetacelloAtomicConfigurationResource >> version50MethodSourceFoo [

	^(self class sourceCodeAt: #version50Foo:) asString


]

{ #category : #ConfigurationOfUmbrella }
MetacelloAtomicConfigurationResource >> version50MethodSourceUmbrella [

	^(self class sourceCodeAt: #version50Umbrella:) asString


]

{ #category : #ConfigurationOfUmbrella }
MetacelloAtomicConfigurationResource >> version50Umbrella: spec [ 
	"self reset"
	<version: '5.0'>
	
	spec for: #common do: [
		spec 
			project: 'Umbrella' with: [
				spec
					className: 'MetacelloTestConfigurationOfLinearFoo';
					versionString: '5.0';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself ].

]

{ #category : #ConfigurationOfAtomicFoo }
MetacelloAtomicConfigurationResource >> version51Foo: spec [ 
	"self reset"
	<version: '5.1'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec blessing: #release.
		spec 
			package: 'GoferUmbrella' with: 'GoferUmbrella-lr.5';
			yourself ].

]

{ #category : #ConfigurationOfAtomicFoo }
MetacelloAtomicConfigurationResource >> version51MethodSourceFoo [

	^(self class sourceCodeAt: #version51Foo:) asString


]

{ #category : #ConfigurationOfUmbrella }
MetacelloAtomicConfigurationResource >> version51MethodSourceUmbrella [

	^(self class sourceCodeAt: #version51Umbrella:) asString


]

{ #category : #ConfigurationOfUmbrella }
MetacelloAtomicConfigurationResource >> version51Umbrella: spec [ 
	"self reset"
	<version: '5.1'>
	
	spec for: #common do: [
		spec 
			project: 'Umbrella' with: [
				spec
					className: 'MetacelloTestConfigurationOfLinearFoo';
					versionString: '5.1';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself ].

]
