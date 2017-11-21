Class {
	#name : #MetacelloConfigurationResource,
	#superclass : #TestResource,
	#instVars : [
		'monticelloRepository'
	],
	#classVars : [
		'ProjectAttributes'
	],
	#category : #Metacello-TestsMCResources
}

{ #category : #accessing }
MetacelloConfigurationResource class >> project [

	^self new project
]

{ #category : #accessing }
MetacelloConfigurationResource class >> projectAttributes [

	ProjectAttributes == nil ifTrue: [ ^#() ].
	^ProjectAttributes
]

{ #category : #accessing }
MetacelloConfigurationResource class >> projectAttributes: aCollection [

	ProjectAttributes := aCollection
]

{ #category : #'Issue 77' }
MetacelloConfigurationResource >> baseline10B: spec [ 
	"self reset"
	<version: '1.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			project: 'C' with: [
				spec
					className: 'MetacelloTestConfigurationOfIssue77C';
					loads: #('GoferFoo');
					versionString: '1.2';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			package: 'GoferBeau' with: 'GoferBeau-dkh.53';
			yourself ].

]

{ #category : #'Issue 77' }
MetacelloConfigurationResource >> baseline10D: spec [ 
	"self reset"
	<version: '1.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			project: 'C' with: [
				spec
					className: 'MetacelloTestConfigurationOfIssue77C';
					loads: #('GoferFoo');
					versionString: '1.3';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			package: 'GoferBar' with: [
				spec 
					file:'GoferBar-lr.1' ];
			group: 'default' with: #('GoferBar');
			yourself ].

]

{ #category : #'Issue 77' }
MetacelloConfigurationResource >> baseline10MethodSourceB [

	^(self class sourceCodeAt: #baseline10B:) asString


]

{ #category : #'Issue 77' }
MetacelloConfigurationResource >> baseline10MethodSourceD [

	^(self class sourceCodeAt: #baseline10D:) asString


]

{ #category : #ConfigurationOfProjectFum }
MetacelloConfigurationResource >> baseline10ProjectFum: spec [
    "self reset"

    <version: '1.0'>
    spec
        for: #'common'
        do: [ 
            spec
                project: 'Foo'
                    with: [ 
                            spec
                                className: 'MetacelloTestConfigurationOfFoo';
                                versionString: '5.0';
                                repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
                project: 'Fie'
                    with: [ 
                            spec
                                className: 'MetacelloTestConfigurationOfProjectFie';
                                versionString: '2.5';
                                repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
                yourself ]
]

{ #category : #'Issue 77' }
MetacelloConfigurationResource >> baseline12C: spec [ 
	"self reset"
	<version: '1.2'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			package: 'GoferFoo' with: 'GoferFoo-lr.1';
			package: 'GeauxBeau' with: 'GeauxBeau-dkh.55';
			yourself. ].

]

{ #category : #'Issue 77' }
MetacelloConfigurationResource >> baseline12MethodSourceC [

	^(self class sourceCodeAt: #baseline12C:) asString


]

{ #category : #'Issue 77' }
MetacelloConfigurationResource >> baseline13C: spec [ 
	"self reset"
	<version: '1.3'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			package: 'GoferFoo' with: 'GoferFoo-lr.4';
			package: 'GeauxBeau' with: 'GeauxBeau-dkh.56';
			yourself ].

]

{ #category : #'Issue 77' }
MetacelloConfigurationResource >> baseline13MethodSourceC [

	^(self class sourceCodeAt: #baseline13C:) asString


]

{ #category : #ConfigurationOfProjectFie }
MetacelloConfigurationResource >> baseline15ProjectFie: spec [
  "self reset"

  <version: '1.5'>
  spec
    for: #'common'
    do: [ 
      spec
        configuration: 'Foo'
        with: [ 
          spec
            className: 'MetacelloTestConfigurationOfFoo';
            versionString: '3.5';
            repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ]
]

{ #category : #ConfigurationOfFan }
MetacelloConfigurationResource >> baseline20Fan: spec [ 
	"self reset"
	<version: '2.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			project: 'Foo' with: [
				spec
					className: 'MetacelloTestConfigurationOfFoo';
					versionString: '2.0';
					loads: #('GoferFaux' 'GoferBeau');
					file: 'MetacelloTestConfigurationOfFoo';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			project: 'Far' with: [
				spec
					className: 'MetacelloTestConfigurationOfFoo';
					versionString: '2.0';
					loads: #('GoferBeau');
					file: 'MetacelloTestConfigurationOfFoo';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself ].

]

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> baseline20Foo: spec [ 
	"self reset"
	<version: '2.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			package: 'GoferFoo' with: 'GoferFoo-lr.1';
			package: 'GoferBar' with: 'GoferBar.branch-lr.1';
			package: 'GoferFaux' with: 'GoferFaux-tg.30';
			package: 'GoferBeau' with: 'GoferBeau-dkh.25';
			yourself ].

]

{ #category : #ConfigurationOfFan }
MetacelloConfigurationResource >> baseline20MethodSourceFan [

	^(self class sourceCodeAt: #baseline20Fan:) asString


]

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> baseline20MethodSourceFoo [

	^(self class sourceCodeAt: #baseline20Foo:) asString


]

{ #category : #ConfigurationOfProjectFie }
MetacelloConfigurationResource >> baseline25ProjectFie: spec [
  "self reset"

  <version: '2.5'>
  spec
    for: #'common'
    do: [ 
      spec
        configuration: 'Foo'
        with: [ 
          spec
            className: 'MetacelloTestConfigurationOfFoo';
            versionString: '3.5';
            repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ]
]

{ #category : #ConfigurationOfFan }
MetacelloConfigurationResource >> baseline30Fan: spec [ 
	"self reset"
	<version: '3.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			project: 'Foo' with: [
				spec
					className: 'MetacelloTestConfigurationOfFoo';
					versionString: '3.0';
					loads: #('GoferFaux' 'GoferBeau');
					file: 'MetacelloTestConfigurationOfFoo';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself ].

]

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> baseline30Foo: spec [ 
	"self reset"
	<version: '3.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			package: 'GoferFoo' with: [
				spec 
					preLoadDoIt: #preloadDoIt;
					file: 'GoferFoo-lr.1' ];
			package: 'GoferBar' with: 'GoferBar.branch-lr.1';
			package: 'GoferFaux' with: 'GoferFaux-tg.30';
			package: 'GoferBeau' with: 'GoferBeau-dkh.53';
			yourself ].

]

{ #category : #ConfigurationOfFan }
MetacelloConfigurationResource >> baseline30MethodSourceFan [

	^(self class sourceCodeAt: #baseline30Fan:) asString


]

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> baseline30MethodSourceFoo [

	^(self class sourceCodeAt: #baseline30Foo:) asString


]

{ #category : #ConfigurationOfFan }
MetacelloConfigurationResource >> baseline35Fan: spec [ 
	"self reset"
	<version: '3.5'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			project: 'Foo' with: [
				spec
					className: 'MetacelloTestConfigurationOfFoo';
					versionString: '3.5';
					loads: #('GoferFaux' 'GoferBeau');
					file: 'MetacelloTestConfigurationOfFoo';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself ].

]

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> baseline35Foo: spec [
  "self reset"

  <version: '3.5'>
  spec
    for: #'common'
    do: [ 
      spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
      spec
        package: 'GoferFoo' with: 'GoferFoo-lr.1';
        package: 'GoferBar' with: 'GoferBar-lr.1';
        package: 'GoferFaux' with: 'GoferFaux-tg.31';
        package: 'GoferBeau' with: 'GoferBeau-dkh.54';
        yourself.
      spec
        group: '1' with: #('GoferFoo' 'GoferBar');
        group: '2' with: #('GoferFoo' 'GoferFaux');
        yourself ]
]

{ #category : #ConfigurationOfFan }
MetacelloConfigurationResource >> baseline35MethodSourceFan [

	^(self class sourceCodeAt: #baseline35Fan:) asString


]

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> baseline35MethodSourceFoo [

	^(self class sourceCodeAt: #baseline35Foo:) asString


]

{ #category : #ConfigurationOfProjectFie }
MetacelloConfigurationResource >> baseline35ProjectFie: spec [ 
	"just projects"
	"self reset"

	<version: '3.5'>
	spec for: #common do: [
		spec 
			project: 'Foo' with: [
				spec
					className: 'MetacelloTestConfigurationOfFoo';
					versionString: '3.5';
					file: 'MetacelloTestConfigurationOfFoo';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			project: 'Feaux' with: [
				spec
					className: 'MetacelloTestConfigurationOfFeaux';
					versionString: '4.0';
					file: 'MetacelloTestConfigurationOfFeaux';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself. ].

]

{ #category : #ConfigurationOfFan }
MetacelloConfigurationResource >> baseline40Fan: spec [ 
	"self reset"
	<version: '4.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			project: 'Foo' with: [
				spec
					className: 'MetacelloTestConfigurationOfFoo';
					versionString: '4.0';
					loads: #('GoferFaux' 'GoferBeau');
					file: 'MetacelloTestConfigurationOfFoo';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself ].

]

{ #category : #ConfigurationOfFeaux }
MetacelloConfigurationResource >> baseline40Feaux: spec [ 
	"self reset"
	<version: '4.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			package: 'GeauxFoo' with:'GeauxFoo-lr.1';
			package: 'GeauxBar' with: 'GeauxBar.branch-lr.2';
			yourself ].

]

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> baseline40Foo: spec [
  "self reset"

  <version: '4.0'>
  spec
    for: #'common'
    do: [ 
      spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
      spec
        package: 'GoferFoo' with: 'GoferFoo-lr.1';
        package: 'GoferBar' with: 'GoferBar-lr.1';
        package: 'GoferFaux' with: 'GoferFaux-tg.32';
        package: 'GoferBeau' with: 'GoferBeau-dkh.55';
        yourself.
      spec
        group: '1' with: #('GoferFoo' 'GoferBar');
        group: '2' with: #('GoferFoo' 'GoferFaux');
        yourself ]
]

{ #category : #ConfigurationOfFan }
MetacelloConfigurationResource >> baseline40MethodSourceFan [

	^(self class sourceCodeAt: #baseline40Fan:) asString


]

{ #category : #ConfigurationOfFeaux }
MetacelloConfigurationResource >> baseline40MethodSourceFeaux [

	^(self class sourceCodeAt: #baseline40Feaux:) asString


]

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> baseline40MethodSourceFoo [

	^(self class sourceCodeAt: #baseline40Foo:) asString


]

{ #category : #ConfigurationOfProjectFee }
MetacelloConfigurationResource >> baseline40MethodSourceProjectFee [

	^(self class sourceCodeAt: #baseline40ProjectFee:) asString


]

{ #category : #ConfigurationOfProjectFoe }
MetacelloConfigurationResource >> baseline40MethodSourceProjectFoe [

	^(self class sourceCodeAt: #baseline40ProjectFoe:) asString


]

{ #category : #ConfigurationOfFum }
MetacelloConfigurationResource >> baseline40MethodSourceProjectFum [

	^(self class sourceCodeAt: #baseline40ProjectFum:) asString


]

{ #category : #'Issue 95' }
MetacelloConfigurationResource >> baseline40MethodSourceProjectInfinite [

	^(self class sourceCodeAt: #baseline40ProjectInfinite:) asString


]

{ #category : #'Issue 95' }
MetacelloConfigurationResource >> baseline40MethodSourceProjectIssue95 [

	^(self class sourceCodeAt: #baseline40ProjectIssue95:) asString


]

{ #category : #'Issue 95' }
MetacelloConfigurationResource >> baseline40MethodSourceProjectLoop [

	^(self class sourceCodeAt: #baseline40ProjectLoop:) asString


]

{ #category : #ConfigurationOfProjectFee }
MetacelloConfigurationResource >> baseline40ProjectFee: spec [ 
	"projects and packages"
	"self reset"
	<version: '4.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			project: 'Foo' with: [
				spec
					className: 'MetacelloTestConfigurationOfFoo';
					versionString: '4.0';
					file: 'MetacelloTestConfigurationOfFoo';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself.
		spec 
			package: 'GeauxFaux' with: 'GeauxFaux-tg.32';
			package: 'GeauxBeau' with: 'GeauxBeau-dkh.55';
			yourself ].

]

{ #category : #ConfigurationOfProjectFie }
MetacelloConfigurationResource >> baseline40ProjectFie: spec [ 
	"just projects"
	"self reset"

	<version: '4.0'>
	spec for: #common do: [
		spec 
			project: 'Foo' with: [
				spec
					className: 'MetacelloTestConfigurationOfFoo';
					versionString: '4.0';
					file: 'MetacelloTestConfigurationOfFoo';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			project: 'Feaux' with: [
				spec
					className: 'MetacelloTestConfigurationOfFeaux';
					versionString: '4.0';
					file: 'MetacelloTestConfigurationOfFeaux';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself. ].

]

{ #category : #ConfigurationOfProjectFoe }
MetacelloConfigurationResource >> baseline40ProjectFoe: spec [ 
	"projects and packages"
	"self reset"
	<version: '4.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			project: 'Foo' with: [
				spec
					className: 'MetacelloTestConfigurationOfFoo';
					versionString: '4.0';
					loads: '1';
					file: 'MetacelloTestConfigurationOfFoo';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself ].

]

{ #category : #ConfigurationOfFum }
MetacelloConfigurationResource >> baseline40ProjectFum: spec [ 
	"master"
	"self reset"
	<version: '4.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			project: 'Feaux' with: [
				spec
					className: 'MetacelloTestConfigurationOfFeaux';
					versionString: '4.0';
					file: 'MetacelloTestConfigurationOfFeaux';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			project: 'Foo' with: [
				spec
					className: 'MetacelloTestConfigurationOfFoo';
					versionString: '4.0';
					file: 'MetacelloTestConfigurationOfFoo';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			project: 'ProjectFie' with: [
				spec
					className: 'MetacelloTestConfigurationOfProjectFie';
					versionString: '4.0';
					file: 'MetacelloTestConfigurationOfProjectFie';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			project: 'ProjectFee' with: [
				spec
					className: 'MetacelloTestConfigurationOfProjectFee';
					versionString: '4.0';
					file: 'MetacelloTestConfigurationOfProjectFee';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself ].

]

{ #category : #'Issue 95' }
MetacelloConfigurationResource >> baseline40ProjectInfinite: spec [ 
	"self reset"
	<version: '4.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec
			project: 'Loop' with: [
				spec
					className: 'MetacelloTestConfigurationOfProjectLoop';
					versionString: '4.0';
					loads: 'GeauxBeau';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself.
		spec 
			package: 'GeauxBar' with: [
				spec
					file: 'GeauxBar.branch-lr.2';
					requires: 'Loop';
					yourself ];
			package: 'GoferBar' with: 'GoferBar-lr.1';
			yourself ].
]

{ #category : #'Issue 95' }
MetacelloConfigurationResource >> baseline40ProjectIssue95: spec [ 
	"self reset"
	<version: '4.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			project: 'Loop' with: [
				spec
					className: 'MetacelloTestConfigurationOfProjectLoop';
					versionString: '4.0';
					loads: 'GoferFaux';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			project: 'Infinite' with: [
				spec
					className: 'MetacelloTestConfigurationOfProjectInfinite';
					versionString: '4.0';
					loads: 'GeauxBar';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself ].
]

{ #category : #'Issue 95' }
MetacelloConfigurationResource >> baseline40ProjectLoop: spec [ 
	"self reset"
	<version: '4.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			project: 'Infinite' with: [
				spec
					className: 'MetacelloTestConfigurationOfProjectInfinite';
					versionString: '4.0';
					loads: 'GoferBar';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			package: 'GoferFaux' with: [
				spec
					file:'GoferFaux-tg.30';
					requires: 'Infinite';
					yourself ];
			package: 'GeauxBeau' with: [
				spec
					file: 'GeauxBeau-dkh.55';
					yourself ];
			yourself ].

]

{ #category : #ConfigurationOfFan }
MetacelloConfigurationResource >> baseline50Fan: spec [ 
	"self reset"
	<version: '5.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			project: 'Foo' with: [
				spec
					className: 'MetacelloTestConfigurationOfFoo';
					versionString: '5.0';
					loads: #('GoferFaux' 'GoferBeau');
					file: 'MetacelloTestConfigurationOfFoo';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself ].

]

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> baseline50Foo: spec [ 
	"self reset"
	<version: '5.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			package: 'GoferFoo' with:'GoferFoo-lr.2';
			package: 'GoferBar' with: 'GoferBar-lr.1';
			package: 'GoferFaux' with: 'GoferFaux-tg.34';
			package: 'GoferBeau' with: 'GoferBeau-dkh.55';
			yourself.
		spec
			group: '1' with: #('GoferFoo' 'GoferBar');
			group: '2' with: #('GoferFoo' 'GoferFaux');
			yourself ].

]

{ #category : #ConfigurationOfFan }
MetacelloConfigurationResource >> baseline50MethodSourceFan [

	^(self class sourceCodeAt: #baseline50Fan:) asString


]

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> baseline50MethodSourceFoo [

	^(self class sourceCodeAt: #baseline50Foo:) asString


]

{ #category : #ConfigurationOfFix }
MetacelloConfigurationResource >> baseline60Fix: spec [ 
	"self reset"
	<version: '6.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			package: 'GoferFoo' with: 'GoferFoo-lr.4';
			package: 'GoferBar' with: 'GoferBar-lr.1';
			package: 'GoferFaux' with: 'GoferFaux-tg.35';
			yourself. ].

]

{ #category : #ConfigurationOfFix }
MetacelloConfigurationResource >> baseline60MethodSourceFix [

	^(self class sourceCodeAt: #baseline60Fix:) asString


]

{ #category : #ConfigurationOfFix }
MetacelloConfigurationResource >> baseline61Fix: spec [ 
	"self reset"
	<version: '6.1'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			package: 'GoferFoo' with: 'GoferFoo-lr.4';
			package: 'GoferBar' with: 'GoferBar-lr.1';
			package: 'GoferBeau' with: 'GoferBeau-dkh.53';
			package: 'GoferFaux' with: 'GoferFaux-tg.31';
			yourself. ].

]

{ #category : #ConfigurationOfFix }
MetacelloConfigurationResource >> baseline61MethodSourceFix [

	^(self class sourceCodeAt: #baseline61Fix:) asString


]

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> baselineGoo300Issue156Configuration: spec [
  "https://github.com/dalehenrich/metacello-work/issues/156"

  <version: '3.0.0'>
  spec
    for: #'common'
    do: [ 
      spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
      spec
        package: 'GoferGoo' with: 'GoferFoo-lr.1';
        package: 'GoferBar' with: 'GoferBar.branch-lr.1';
        package: 'GoferFaux' with: 'GoferFaux-tg.30';
        package: 'GoferBeau' with: 'GoferBeau-dkh.53';
        yourself ]
]

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> baselineGoo400Issue156Configuration: spec [
  "https://github.com/dalehenrich/metacello-work/issues/156"

  <version: '4.0.0'>
  spec
    for: #'common'
    do: [ 
      spec
        baseline: 'Goo'
          with: [ spec repository: 'dictionary://Metacello_MczConfiguration_Test_Repository' ];
        import: 'Goo' ]
]

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> baselineGoo500Issue156Configuration: spec [
  "https://github.com/dalehenrich/metacello-work/issues/156"

  <version: '5.0.0'>
  spec
    for: #'common'
    do: [ 
      spec
        baseline: 'Goo'
          with: [ spec repository: 'dictionary://Metacello_MczConfiguration_Test_Repository' ];
        import: 'Goo' ]
]

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> baselineGooIssue156Baseline: spec [
  "https://github.com/dalehenrich/metacello-work/issues/156"

  <baseline>
  spec
    for: #'common'
    do: [ 
      spec
        package: 'GoferFoo' with: 'GoferFoo-lr.1';
        package: 'GoferBar' with: 'GoferBar.branch-lr.1';
        package: 'GoferFaux' with: 'GoferFaux-tg.30';
        package: 'GoferBeau' with: 'GoferBeau-dkh.53';
        yourself ]
]

{ #category : #'Issue 115' }
MetacelloConfigurationResource >> baselineVersion10Issue115: spec [
    <version: '1.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
            spec
                project: 'Foo'
                    with: [ 
                            spec
                                versionString: #'bleedingEdge';
                                className: 'MetacelloTestConfigurationOfFoo';
                                repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
                yourself.
            spec package: 'GeauxFoo' ]
]

{ #category : #'Issue 119' }
MetacelloConfigurationResource >> baselineVersion10Issue119: spec [
    <version: '1.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
            spec
                project: 'Foo '
                    with: [ 
                            spec
                                className: 'MetacelloTestConfigurationOfFoo';
                                repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
                yourself.
            spec package: 'GoferBar ' ]
]

{ #category : #'Issue 136' }
MetacelloConfigurationResource >> baselineVersion10Issue136: spec [
	<version: '1.0-baseline'>

	spec for: #'common' do: [
		spec blessing: #baseline.
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			package: 'GoferBeau'. ].

]

{ #category : #'Issue 154' }
MetacelloConfigurationResource >> baselineVersion10Issue154: spec [
    <version: '1.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec
                project: 'MetacelloExample'
                    with: [ 
                            spec
                                className: 'ConfigurationOfMetacelloExample';
                                versionString: '1.0';
                                projectPackage: [ 
                                            spec
                                                name: 'ConfigurationOfMetacelloExample';
                                                repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ];
                yourself ]
]

{ #category : #'Issue 154' }
MetacelloConfigurationResource >> baselineVersion10MetacelloExample: spec [
    <version: '1.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
            spec package: 'GoferFoo' ]
]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> baselineVersion113ProjectToolBox: spec [
	<version: '1.1.3-baseline'>

	spec for: #'outer' do: [
		spec 
			package: 'Example-Core';
			package: 'Example-Test'. ].

]

{ #category : #'Issue 154' }
MetacelloConfigurationResource >> baselineVersion11Issue154: spec [
    <version: '1.1-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec
                project: 'MetacelloExample'
                    with: [ 
                            spec
                                className: 'ConfigurationOfMetacelloExample';
                                versionString: '1.0';
                                projectPackage: [ 
                                            spec
                                                name: 'MetacelloExampleTestConfigurationIssue154';
                                                repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ];
                yourself ]
]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> baselineVersion124ProjectToolBox: spec [
	<version: '1.2.4-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'http://www.example.com/aa'. ].

	spec for: #'common' do: [
		spec repository: 'http://www.example.com/ab'. ].

]

{ #category : #'Issue 154' }
MetacelloConfigurationResource >> baselineVersion12Issue154: spec [
    <version: '1.2-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec
                project: 'MetacelloExample'
                    with: [ 
                            spec
                                className: 'ConfigurationOfMetacelloExample';
                                versionString: '1.0';
                                projectPackage: [ 
                                            spec
                                                name: 'MetacelloExampleTestConfigurationIssue154';
                                                file: 'MetacelloExampleTestConfigurationIssue154.gemstone';
                                                repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ];
                yourself ]
]

{ #category : #'Issue 154' }
MetacelloConfigurationResource >> baselineVersion13Issue154: spec [
    <version: '1.3-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec
                project: 'MetacelloExample'
                    with: [ 
                            spec
                                className: 'ConfigurationOfMetacelloExample';
                                versionString: '1.0';
                                projectPackage: [ 
                                            spec
                                                file: 'MetacelloExampleTestConfigurationIssue154.gemstone';
                                                repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ];
                yourself ]
]

{ #category : #'Issue 119' }
MetacelloConfigurationResource >> baselineVersion20Issue119: spec [
    <version: '2.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
            spec
                project: 'Foo'
                    with: [ 
                            spec
                                versionString: #'bleedingEdge';
                                className: 'MetacelloTestConfigurationOfFoo';
                                repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
                yourself.
            spec package: 'GoferBar ' ]
]

{ #category : #'Issue 125' }
MetacelloConfigurationResource >> baselineVersion20Issue125: spec [
    <version: '2.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec description: 'MetacelloConfigurationResource>>baselineVersion20Issue125:'.
            spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
            spec
                package: 'GoferFoo';
                package: 'GoferBar' with: [ spec requires: 'GoferFoo' ];
                yourself ]
]

{ #category : #'Issue 154' }
MetacelloConfigurationResource >> baselineVersion20Issue154: spec [
    <version: '2.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec
                project: 'MetacelloExample'
                    with: [ 
                            spec
                                className: 'ConfigurationOfMetacelloExample';
                                versionString: '1.0';
                                repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
                yourself ]
]

{ #category : #'Issue 171' }
MetacelloConfigurationResource >> baselineVersion20Issue171: spec [
    <version: '2.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
            spec
                project: 'Foo'
                    with: [ 
                            spec
                                className: 'MetacelloTestConfigurationOfFoo';
                                repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
                yourself.
            spec package: 'GeauxFoo' ]
]

{ #category : #'Issue 171' }
MetacelloConfigurationResource >> baselineVersion21Issue171: spec [
    <version: '2.1-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
            spec
                project: 'Foo' with: [ spec className: 'MetacelloTestConfigurationOfFoo' ];
                yourself.
            spec package: 'GeauxFoo' ]
]

{ #category : #'Issue 119' }
MetacelloConfigurationResource >> baselineVersion30Issue119: spec [
    <version: '3.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
            spec
                project: 'Foo '
                    with: [ 
                            spec
                                versionString: #'bleedingEdge';
                                className: 'MetacelloTestConfigurationOfFoo';
                                repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
                yourself.
            spec package: 'GoferBar' ]
]

{ #category : #'Issue 125' }
MetacelloConfigurationResource >> baselineVersion30Issue125: spec [
    <version: '3.0-baseline' imports: #('2.0-baseline')>
    spec
        for: #(#'attribute1' #'attribute2')
        do: [ 
            spec description: 'MetacelloConfigurationResource>>baselineVersion30Issue125:'.
            spec
                package: 'GoferFaux' with: [ spec requires: 'GoferFoo' ];
                package: 'GoferBeau' with: [ spec requires: 'GoferFaux' ];
                yourself ]
]

{ #category : #'Issue 154' }
MetacelloConfigurationResource >> baselineVersion30Issue154: spec [
    <version: '3.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec
                project: 'MetacelloExample'
                    with: [ 
                            spec
                                className: 'ConfigurationOfMetacelloExample';
                                versionString: '1.0';
                                repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
                yourself ]
]

{ #category : #'Issue 119' }
MetacelloConfigurationResource >> baselineVersion40Issue119: spec [
    <version: '4.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
            spec
                project: 'Foo'
                    with: [ 
                            spec
                                versionString: #'bleedingEdge';
                                className: 'MetacelloTestConfigurationOfFoo';
                                repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
                yourself.
            spec package: 'GoferBar' ]
]

{ #category : #'issue 283' }
MetacelloConfigurationResource >> bleedingEdge206A: spec [
  "self reset"

  <symbolicVersion: #'bleedingEdge'>
  spec for: #'common' version: '2.0'
]

{ #category : #'issue 283' }
MetacelloConfigurationResource >> bleedingEdge206B: spec [
  "self reset"

  <symbolicVersion: #'bleedingEdge'>
  spec for: #'common' version: '2.1'
]

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> bleedingEdgeVersion: spec [
    "self reset"

    <symbolicVersion: #'bleedingEdge'>
    spec for: #'common' version: '4.0'
]

{ #category : #accessing }
MetacelloConfigurationResource >> monticelloRepository [
	^ monticelloRepository
]

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> postloadDoIt [
	"self reset"

	Smalltalk at: #'Metacello_Configuration_Test_POST_DoIt_Result' put: true
]

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> preloadDoIt [
	"self reset"

	Smalltalk at: #'Metacello_Configuration_Test_DoIt_Result' put: true
]

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> preloadDoItMethodSource [

	^(self class sourceCodeAt: #preloadDoIt) asString


]

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> project [
	"self reset"

	| project |
	"Construct Metacello project"
	project := MetacelloMCProject new.
	"Allow for customization of #projectAttributes"
	project projectAttributes: MetacelloConfigurationResource projectAttributes.
	MetacelloVersionConstructor on: self project: project.
	project loader: 
		((project loaderClass new)
			shouldDisablePackageCache: true;
			yourself).
	project loadType: #linear.
	^project

]

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> projectClassMethodSource [

	^(self class class sourceCodeAt: #project) asString


]

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> projectMethodSource [

	^(self class sourceCodeAt: #project) asString


]

{ #category : #'issue 283' }
MetacelloConfigurationResource >> projectVersion10Issue283: spec [
  <version: '1.0'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'release'.
      spec
        configuration: 'ProjectIssue283'
        with: [ 
          spec
            version: #'stable';
            repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ]
]

{ #category : #'issue 283' }
MetacelloConfigurationResource >> projectVersion20Issue283: spec [
  <version: '2.0'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'release'.
      spec
        configuration: 'ProjectIssue283'
        with: [ 
          spec
            version: #'bleedingEdge';
            repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ]
]

{ #category : #running }
MetacelloConfigurationResource >> setUp [
  "self reset"

  super setUp.
  self
    setUpMonticelloRepository;
    setUpConfigurationOfFoo;
    setUpConfigurationOfFeaux;
    setUpConfigurationOfProjectFie;
    setUpConfigurationOfProjectFee;
    setUpConfigurationOfFum;
    setUpConfigurationOfProjectFoe;
    setUpConfigurationOfFan;
    setUpConfigurationOfFix;
    setUpIssue77;
    setUpIssue95;
    setUpConfigurationOfSymbolic;
    setUpConfigurationOfProjectToolBox;
    setUpConfigurationOfProjectIssue115;
    setUpConfigurationOfProjectIssue136;
    setUpConfigurationOfProjectIssue119;
    setUpConfigurationOfProjectIssue171;
    setUpConfigurationOfProjectIssue154;
    setUpConfigurationOfProjectIssue156;
    setUpIssue156ConfigurationOfProjectGoo;
    setUpIssue156ConfigurationOfProjectSoo;
    setUpConfigurationOfProjectFum;
    setUpConfigurationOfProjectIssue125;
    setUpConfigurationOfProjectIssue283;
    yourself
]

{ #category : #ConfigurationOfFan }
MetacelloConfigurationResource >> setUpConfigurationOfFan [
	"self reset"
	
	| reference className definitionArray |
	reference := GoferVersionReference name: 'MetacelloTestConfigurationOfFan-dkh.1'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: className).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectClassMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline20Fan:'
						category: 'cat'
						timeStamp: ''
						source: self baseline20MethodSourceFan.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline30Fan:'
						category: 'cat'
						timeStamp: ''
						source: self baseline30MethodSourceFan.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline35Fan:'
						category: 'cat'
						timeStamp: ''
						source: self baseline35MethodSourceFan.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline40Fan:'
						category: 'cat'
						timeStamp: ''
						source: self baseline40MethodSourceFan.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline50Fan:'
						category: 'cat'
						timeStamp: ''
						source: self baseline50MethodSourceFan.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MetacelloTestsMCPackage new name: reference packageName)
		info: (MCVersionInfo
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

{ #category : #ConfigurationOfFeaux }
MetacelloConfigurationResource >> setUpConfigurationOfFeaux [
	"self reset"
	
	| reference className definitionArray |
	reference := GoferVersionReference name: 'MetacelloTestConfigurationOfFeaux-dkh.1'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: className).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectClassMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline40Feaux:'
						category: 'cat'
						timeStamp: ''
						source: self baseline40MethodSourceFeaux.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MetacelloTestsMCPackage new name: reference packageName)
		info: (MCVersionInfo
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

{ #category : #ConfigurationOfFix }
MetacelloConfigurationResource >> setUpConfigurationOfFix [
	"self reset"
	
	| reference className definitionArray |
	reference := GoferVersionReference name: 'MetacelloTestConfigurationOfFix-dkh.1'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: className).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectClassMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline60Fix:'
						category: 'cat'
						timeStamp: ''
						source: self baseline60MethodSourceFix.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline61Fix:'
						category: 'cat'
						timeStamp: ''
						source: self baseline61MethodSourceFix.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MetacelloTestsMCPackage new name: reference packageName)
		info: (MCVersionInfo
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

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> setUpConfigurationOfFoo [
    "self reset"

    | reference className definitionArray |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfFoo-dkh.1'.
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
        classIsMeta: false
        selector: 'bleedingEdgeVersion:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'bleedingEdgeVersion:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'project'
        category: 'cat'
        timeStamp: ''
        source: self projectMethodSource).
    (MCMethodDefinition
        className: className asString
        classIsMeta: true
        selector: 'project'
        category: 'cat'
        timeStamp: ''
        source: self projectClassMethodSource).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline20Foo:'
        category: 'cat'
        timeStamp: ''
        source: self baseline20MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline30Foo:'
        category: 'cat'
        timeStamp: ''
        source: self baseline30MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline35Foo:'
        category: 'cat'
        timeStamp: ''
        source: self baseline35MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline40Foo:'
        category: 'cat'
        timeStamp: ''
        source: self baseline40MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline50Foo:'
        category: 'cat'
        timeStamp: ''
        source: self baseline50MethodSourceFoo).
    (MCMethodDefinition
        className: className asString
        selector: 'preloadDoIt'
        category: 'cat'
        timeStamp: ''
        source: self preloadDoItMethodSource)}.
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

{ #category : #ConfigurationOfFum }
MetacelloConfigurationResource >> setUpConfigurationOfFum [
	"self reset"
	
	| reference className definitionArray |
	reference := GoferVersionReference name: 'MetacelloTestConfigurationOfFum-dkh.1'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: className).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectClassMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline40ProjectFum:'
						category: 'cat'
						timeStamp: ''
						source: self baseline40MethodSourceProjectFum.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MetacelloTestsMCPackage new name: reference packageName)
		info: (MCVersionInfo
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

{ #category : #'Issue 154' }
MetacelloConfigurationResource >> setUpConfigurationOfMetacelloExampledkh1 [
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'ConfigurationOfMetacelloExample-dkh.1'.
    className := reference packageName asSymbol.
    definitionArray := {(MCClassDefinition
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
        selector: 'baselineVersion10MetacelloExample:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion10MetacelloExample:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version10MetacelloExample:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version10MetacelloExample:') asString)}.
    monticelloRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'issue 283' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectConfigIssue283dkh1 [
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference
    name: 'ConfigurationOfProjectConfigIssue283-dkh.1'.
  className := reference packageName asSymbol.
  definitionArray := {(MCClassDefinition
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
    selector: 'projectVersion10Issue283:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'projectVersion10Issue283:') asString).
  (MCMethodDefinition
    className: className asString
    selector: 'projectVersion20Issue283:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'projectVersion20Issue283:') asString)}.
  monticelloRepository
    basicStoreVersion:
      (MCVersion new
        setPackage: (MetacelloTestsMCPackage new name: reference packageName)
        info:
          (versionInfo := MCVersionInfo
            name: reference name
            id: UUID new
            message: 'This is a mock version'
            date: Date today
            time: Time now
            author: reference author
            ancestors: #())
        snapshot: (MCSnapshot fromDefinitions: definitionArray)
        dependencies: #()).
  ^ versionInfo
]

{ #category : #ConfigurationOfProjectFee }
MetacelloConfigurationResource >> setUpConfigurationOfProjectFee [
	"self reset"
	
	| reference className definitionArray |
	reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectFee-dkh.1'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: className).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectClassMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline40ProjectFee:'
						category: 'cat'
						timeStamp: ''
						source: self baseline40MethodSourceProjectFee.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MetacelloTestsMCPackage new name: reference packageName)
		info: (MCVersionInfo
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

{ #category : #ConfigurationOfProjectFie }
MetacelloConfigurationResource >> setUpConfigurationOfProjectFie [
    "self reset"

    | reference className definitionArray |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectFie-dkh.1'.
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
        selector: 'baseline15ProjectFie:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baseline15ProjectFie:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline25ProjectFie:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baseline25ProjectFie:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline35ProjectFie:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baseline35ProjectFie:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baseline40ProjectFie:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baseline40ProjectFie:') asString)}.
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

{ #category : #ConfigurationOfProjectFoe }
MetacelloConfigurationResource >> setUpConfigurationOfProjectFoe [
	"self reset"
	
	| reference className definitionArray |
	reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectFoe-dkh.1'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: className).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectClassMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline40ProjectFoe:'
						category: 'cat'
						timeStamp: ''
						source: self baseline40MethodSourceProjectFoe.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MetacelloTestsMCPackage new name: reference packageName)
		info: (MCVersionInfo
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

{ #category : #ConfigurationOfProjectFum }
MetacelloConfigurationResource >> setUpConfigurationOfProjectFum [
    "self reset"

    | reference className definitionArray |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectFum-dkh.1'.
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
        selector: 'baseline10ProjectFum:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baseline10ProjectFum:') asString)}.
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

{ #category : #'Issue 95' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectInfinite [
	"self reset"
	
	| reference className definitionArray |
	reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectInfinite-dkh.1'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: className).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectClassMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline40ProjectInfinite:'
						category: 'cat'
						timeStamp: ''
						source: self baseline40MethodSourceProjectInfinite.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MetacelloTestsMCPackage new name: reference packageName)
		info: (MCVersionInfo
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

{ #category : #'Issue 115' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue115 [
	"self reset"
	
	| versionInfo |
	versionInfo := self setUpConfigurationOfProjectIssue115dkh1.
	versionInfo := self setUpConfigurationOfProjectIssue115dkh2: {versionInfo}.
	
]

{ #category : #'Issue 115' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue115dkh1 [
	"self reset"
	
	| reference className definitionArray versionInfo |
	reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectIssue115-dkh.1'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: className).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectClassMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'baselineVersion10Issue115:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #baselineVersion10Issue115:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version10Issue115:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version10Issue115:) asString.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MetacelloTestsMCPackage new name: reference packageName)
		info: (versionInfo := MCVersionInfo
				name: reference name
				id: UUID new
				message: 'This is a mock version'
				date: Date today
				time: Time now
				author: reference author
				ancestors: #())
		snapshot: (MCSnapshot fromDefinitions: definitionArray)
		dependencies: #()).
	^versionInfo
]

{ #category : #'Issue 115' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue115dkh2: ancestors [
	"self reset"
	
	| reference className definitionArray versionInfo |
	reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectIssue115-dkh.2'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: className).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectClassMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'baselineVersion10Issue115:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #baselineVersion10Issue115:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version10Issue115:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version10Issue115:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version11Issue115:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version11Issue115:) asString.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MetacelloTestsMCPackage new name: reference packageName)
		info: (versionInfo := MCVersionInfo
				name: reference name
				id: UUID new
				message: 'This is a mock version'
				date: Date today
				time: Time now
				author: reference author
				ancestors: ancestors)
		snapshot: (MCSnapshot fromDefinitions: definitionArray)
		dependencies: #()).
	^versionInfo

]

{ #category : #'Issue 119' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue119 [
    "self reset"

    | versionInfo |
    versionInfo := self setUpConfigurationOfProjectIssue119dkh1.
    versionInfo := self setUpConfigurationOfProjectIssue119dkh2: {versionInfo}.
    versionInfo := self setUpConfigurationOfProjectIssue119dkh3: {versionInfo}.
    versionInfo := self setUpConfigurationOfProjectIssue119dkh4: {versionInfo}.
    versionInfo := self setUpConfigurationOfProjectIssue119dkh5: {versionInfo}.
    versionInfo := self setUpConfigurationOfProjectIssue119dkh6: {versionInfo}.
    versionInfo := self setUpConfigurationOfProjectIssue119dkh7: {versionInfo}
]

{ #category : #'Issue 119' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue119dkh1 [
    "version 1.0"

    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectIssue119-dkh.1'.
    className := reference packageName asSymbol.
    definitionArray := {(MCClassDefinition
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
        selector: 'baselineVersion10Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion10Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version10Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version10Issue119:') asString)}.
    monticelloRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'Issue 119' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue119dkh2: ancestors [
    "version 2.0"

    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectIssue119-dkh.2'.
    className := reference packageName asSymbol.
    definitionArray := {(MCClassDefinition
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
        selector: 'baselineVersion20Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion20Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version20Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version20Issue119:') asString)}.
    monticelloRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: ancestors)
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'Issue 119' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue119dkh3: ancestors [
    "version 3.0"

    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectIssue119-dkh.3'.
    className := reference packageName asSymbol.
    definitionArray := {(MCClassDefinition
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
        selector: 'baselineVersion30Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion30Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version30Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version30Issue119:') asString)}.
    monticelloRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: ancestors)
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'Issue 119' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue119dkh4: ancestors [
    "version 4.0"

    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectIssue119-dkh.4'.
    className := reference packageName asSymbol.
    definitionArray := {(MCClassDefinition
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
        selector: 'baselineVersion40Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion40Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version40Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version40Issue119:') asString)}.
    monticelloRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: ancestors)
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'Issue 119' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue119dkh5: ancestors [
    "versions, 4.0, 4.1, 4.2, 4.3, 4.4"

    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectIssue119-dkh.5'.
    className := reference packageName asSymbol.
    definitionArray := {(MCClassDefinition
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
        selector: 'baselineVersion40Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion40Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version40Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version40Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version41Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version41Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version42Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version42Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version43Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version43Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version44Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version44Issue119:') asString)}.
    monticelloRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: ancestors)
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'Issue 119' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue119dkh6: ancestors [
    "versions 1.0, 2.0. 3.0,4.0, 4.1, 4.2, 4.3, 4.4"

    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectIssue119-dkh.6'.
    className := reference packageName asSymbol.
    definitionArray := {(MCClassDefinition
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
        selector: 'baselineVersion10Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion10Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baselineVersion20Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion20Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baselineVersion30Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion30Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baselineVersion40Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion40Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version10Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version10Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version20Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version20Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version30Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version30Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version40Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version40Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version41Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version41Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version42Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version42Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version43Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version43Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version44Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version44Issue119:') asString)}.
    monticelloRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: ancestors)
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'Issue 119' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue119dkh7: ancestors [
    "versions 5.0"

    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectIssue119-dkh.7'.
    className := reference packageName asSymbol.
    definitionArray := {(MCClassDefinition
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
        selector: 'baselineVersion40Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion40Issue119:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version50Issue119:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version50Issue119:') asString)}.
    monticelloRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: ancestors)
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'Issue 125' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue125 [
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectIssue125-dkh.1'.
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
        selector: 'version30Issue125:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version30Issue125:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baselineVersion20Issue125:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion20Issue125:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baselineVersion30Issue125:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion30Issue125:') asString)}.
    monticelloRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'Issue 136' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue136 [
	"self reset"
	
	| versionInfo |
	versionInfo := self setUpConfigurationOfProjectIssue136dkh1.
	versionInfo := self setUpConfigurationOfProjectIssue136dkh2: {versionInfo}.
	
]

{ #category : #'Issue 136' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue136dkh1 [
	"self reset"
	
	| reference className definitionArray versionInfo |
	reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectIssue136-dkh.1'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: className).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectClassMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'baselineVersion10Issue136:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #baselineVersion10Issue136:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version10Issue136:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version10Issue136:) asString.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MetacelloTestsMCPackage new name: reference packageName)
		info: (versionInfo := MCVersionInfo
				name: reference name
				id: UUID new
				message: 'This is a mock version'
				date: Date today
				time: Time now
				author: reference author
				ancestors: #())
		snapshot: (MCSnapshot fromDefinitions: definitionArray)
		dependencies: #()).
	^versionInfo
]

{ #category : #'Issue 136' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue136dkh2: ancestors [
	"self reset"
	
	| reference className definitionArray versionInfo |
	reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectIssue136-dkh.2'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: className).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectClassMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'baselineVersion10Issue136:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #baselineVersion10Issue136:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version10Issue136:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version10Issue136:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version11Issue136:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version11Issue136:) asString.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MetacelloTestsMCPackage new name: reference packageName)
		info: (versionInfo := MCVersionInfo
				name: reference name
				id: UUID new
				message: 'This is a mock version'
				date: Date today
				time: Time now
				author: reference author
				ancestors: ancestors)
		snapshot: (MCSnapshot fromDefinitions: definitionArray)
		dependencies: #()).
	^versionInfo

]

{ #category : #'Issue 154' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue154 [
    "self reset"

    | versionInfo |
    versionInfo := self setUpConfigurationOfMetacelloExampledkh1.
    versionInfo := self setUpConfigurationOfProjectIssue154dkh1
]

{ #category : #'Issue 154' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue154dkh1 [
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'ConfigurationOfMetacelloProjectIssue154-dkh.1'.
    className := reference packageName asSymbol.
    definitionArray := {(MCClassDefinition
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
        selector: 'baselineVersion10Issue154:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion10Issue154:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baselineVersion11Issue154:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion11Issue154:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baselineVersion12Issue154:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion12Issue154:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baselineVersion13Issue154:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion13Issue154:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baselineVersion20Issue154:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion20Issue154:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baselineVersion30Issue154:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion30Issue154:') asString)}.
    monticelloRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'Issue 119' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue156 [
  "https://github.com/dalehenrich/metacello-work/issues/156"

  "self reset"

  self
    setUpIssue156BaselineOfGoo;
    setUpIssue156ConfigurationOfGoo
]

{ #category : #'Issue 171' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue171 [
    "self reset"

    | versionInfo |
    versionInfo := self setUpConfigurationOfProjectIssue171dkh1.
    versionInfo := self setUpConfigurationOfProjectIssue171dkh2: {versionInfo}
]

{ #category : #'Issue 171' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue171dkh1 [
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectIssue171-dkh.1'.
    className := reference packageName asSymbol.
    definitionArray := {(MCClassDefinition
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
        selector: 'baselineVersion20Issue171:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion20Issue171:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version20Issue171:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version20Issue171:') asString)}.
    monticelloRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'Issue 171' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue171dkh2: ancestors [
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectIssue171-dkh.2'.
    className := reference packageName asSymbol.
    definitionArray := {(MCClassDefinition
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
        selector: 'baselineVersion21Issue171:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion21Issue171:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version21Issue171:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version21Issue171:') asString)}.
    monticelloRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: ancestors)
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'issue 283' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue283 [
  "self reset"

  | versionInfo |
  versionInfo := self setUpConfigurationOfProjectIssue283dkh1.
  versionInfo := self setUpConfigurationOfProjectIssue283dkh2: {versionInfo}.
  self setUpConfigurationOfProjectConfigIssue283dkh1
]

{ #category : #'issue 283' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue283dkh1 [
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference
    name: 'ConfigurationOfProjectIssue283-dkh.1'.
  className := reference packageName asSymbol.
  definitionArray := {(MCClassDefinition
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
    selector: 'stableVersion283A:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'stableVersion283A:') asString).
  (MCMethodDefinition
    className: className asString
    selector: 'bleedingEdge206A:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'bleedingEdge206A:') asString).
  (MCMethodDefinition
    className: className asString
    selector: 'version20Issue283:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'version20Issue283:') asString)}.
  monticelloRepository
    basicStoreVersion:
      (MCVersion new
        setPackage: (MetacelloTestsMCPackage new name: reference packageName)
        info:
          (versionInfo := MCVersionInfo
            name: reference name
            id: UUID new
            message: 'This is a mock version'
            date: Date today
            time: Time now
            author: reference author
            ancestors: #())
        snapshot: (MCSnapshot fromDefinitions: definitionArray)
        dependencies: #()).
  ^ versionInfo
]

{ #category : #'issue 283' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue283dkh2: ancestors [
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference
    name: 'ConfigurationOfProjectIssue283-dkh.2'.
  className := reference packageName asSymbol.
  definitionArray := {(MCClassDefinition
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
    selector: 'stableVersion283B:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'stableVersion283B:') asString).
  (MCMethodDefinition
    className: className asString
    selector: 'bleedingEdge206B:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'bleedingEdge206B:') asString).
  (MCMethodDefinition
    className: className asString
    selector: 'version20Issue283:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'version20Issue283:') asString).
  (MCMethodDefinition
    className: className asString
    selector: 'version21Issue283:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'version21Issue283:') asString)}.
  monticelloRepository
    basicStoreVersion:
      (MCVersion new
        setPackage: (MetacelloTestsMCPackage new name: reference packageName)
        info:
          (versionInfo := MCVersionInfo
            name: reference name
            id: UUID new
            message: 'This is a mock version'
            date: Date today
            time: Time now
            author: reference author
            ancestors: ancestors)
        snapshot: (MCSnapshot fromDefinitions: definitionArray)
        dependencies: #()).
  ^ versionInfo
]

{ #category : #'Issue 95' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectIssue95 [
	"self reset"
	
	| reference className definitionArray |
	reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectIssue95-dkh.1'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: className).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectClassMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline40ProjectIssue95:'
						category: 'cat'
						timeStamp: ''
						source: self baseline40MethodSourceProjectIssue95.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MetacelloTestsMCPackage new name: reference packageName)
		info: (MCVersionInfo
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

{ #category : #'Issue 95' }
MetacelloConfigurationResource >> setUpConfigurationOfProjectLoop [
	"self reset"
	
	| reference className definitionArray |
	reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectLoop-dkh.1'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: className).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectClassMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline40ProjectLoop:'
						category: 'cat'
						timeStamp: ''
						source: self baseline40MethodSourceProjectLoop.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MetacelloTestsMCPackage new name: reference packageName)
		info: (MCVersionInfo
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

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> setUpConfigurationOfProjectToolBox [
    "self reset"

    | reference className definitionArray |
    reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectToolBox-dkh.1'.
    className := reference packageName asSymbol.
    definitionArray := {(MCClassDefinition
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
        selector: 'postloadDoIt'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'postloadDoIt') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'preloadDoIt'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'preloadDoIt') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'symbolicVersionExplicitlyDoesNotExistProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'symbolicVersionExplicitlyDoesNotExistProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version100ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version100ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version101ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version101ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version102ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version102ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version103ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version103ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version104ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version104ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version105ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version105ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version106ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version106ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version107ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version107ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version108ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version108ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version109ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version109ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version110ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version110ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version111ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version111ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version112ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version112ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baselineVersion113ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion113ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version113ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version113ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version114ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version114ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version115ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version115ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version116ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version116ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version117ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version117ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version118ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version118ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version119ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version119ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version120ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version120ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version121ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version121ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version122ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version122ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version123ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version123ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'baselineVersion124ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineVersion124ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version124ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version124ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version125ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version125ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version126ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version126ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version127ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version127ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version128ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version128ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version129ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version129ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version130ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version130ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version131ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version131ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version132ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version132ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version133ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version133ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version134ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version134ProjectToolBox:') asString).
    (MCMethodDefinition
        className: className asString
        selector: 'version135ProjectToolBox:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version135ProjectToolBox:') asString)}.
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

{ #category : #ConfigurationOfSymbolic }
MetacelloConfigurationResource >> setUpConfigurationOfSymbolic [
	"self reset"
	
	| reference className definitionArray |
	reference := GoferVersionReference name: 'MetacelloTestConfigurationOfSymbolic-dkh.1'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: className).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectClassMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'stableVersionD:'
						category: 'cat'
						timeStamp: ''
						source: self stableVersionDMethodSourceSymbolic.
					MCMethodDefinition 
						className: className asString
						selector: 'version40Symbolic:'
						category: 'cat'
						timeStamp: ''
						source: self version40SymbolicMethodSourceSymbolic.
					MCMethodDefinition 
						className: className asString
						selector: 'version41Symbolic:'
						category: 'cat'
						timeStamp: ''
						source: self version41SymbolicMethodSourceSymbolic.
					MCMethodDefinition 
						className: className asString
						selector: 'version42Symbolic:'
						category: 'cat'
						timeStamp: ''
						source: self version42SymbolicMethodSourceSymbolic.
					MCMethodDefinition 
						className: className asString
						selector: 'version43Symbolic:'
						category: 'cat'
						timeStamp: ''
						source: self version43SymbolicMethodSourceSymbolic.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MetacelloTestsMCPackage new name: reference packageName)
		info: (MCVersionInfo
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

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> setUpIssue156BaselineOfGoo [
  "https://github.com/dalehenrich/metacello-work/issues/156"

  "self reset"

  | reference className definitionArray |
  reference := GoferVersionReference name: 'BaselineOfGoo-dkh.1'.
  className := reference packageName asSymbol.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: className)).
  (MCClassDefinition
    name: className
    superclassName: #'BaselineOf'
    category: className
    instVarNames: #()
    comment: '').
  (MCMethodDefinition
    className: className asString
    selector: 'baselineGooIssue156Baseline:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'baselineGooIssue156Baseline:') asString)}.
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

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> setUpIssue156ConfigurationOfGoo [
  "https://github.com/dalehenrich/metacello-work/issues/156"

  "self reset"

  | reference className definitionArray |
  reference := GoferVersionReference name: 'ConfigurationOfGoo-dkh.1'.
  className := reference packageName asSymbol.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: className)).
  (MCClassDefinition
    name: className
    superclassName: #'ConfigurationOf'
    category: className
    instVarNames: #()
    comment: '').
  (MCMethodDefinition
    className: className asString
    selector: 'baselineGoo300Issue156Configuration:'
    category: 'cat'
    timeStamp: ''
    source:
      (self class sourceCodeAt: #'baselineGoo300Issue156Configuration:') asString).
  (MCMethodDefinition
    className: className asString
    selector: 'baselineGoo400Issue156Configuration:'
    category: 'cat'
    timeStamp: ''
    source:
      (self class sourceCodeAt: #'baselineGoo400Issue156Configuration:') asString).
  (MCMethodDefinition
    className: className asString
    selector: 'baselineGoo500Issue156Configuration:'
    category: 'cat'
    timeStamp: ''
    source:
      (self class sourceCodeAt: #'baselineGoo500Issue156Configuration:') asString)}.
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

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> setUpIssue156ConfigurationOfProjectGoo [
  "https://github.com/dalehenrich/metacello-work/issues/156"

  "self reset"

  | reference className definitionArray |
  reference := GoferVersionReference name: 'ConfigurationOfProjectGoo-dkh.1'.
  className := reference packageName asSymbol.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: className)).
  (MCClassDefinition
    name: className
    superclassName: #'ConfigurationOf'
    category: className
    instVarNames: #()
    comment: '').
  (MCMethodDefinition
    className: className asString
    selector: 'version10Issue156:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'version10Issue156:') asString).
  (MCMethodDefinition
    className: className asString
    selector: 'version11Issue156:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'version11Issue156:') asString).
  (MCMethodDefinition
    className: className asString
    selector: 'version20Issue156:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'version20Issue156:') asString).
  (MCMethodDefinition
    className: className asString
    selector: 'version30Issue156:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'version30Issue156:') asString)}.
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

{ #category : #ConfigurationOfFoo }
MetacelloConfigurationResource >> setUpIssue156ConfigurationOfProjectSoo [
  "self reset"

  | reference className definitionArray |
  reference := GoferVersionReference name: 'ConfigurationOfProjectSoo-dkh.1'.
  className := reference packageName asSymbol.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: className)).
  (MCClassDefinition
    name: className
    superclassName: #'ConfigurationOf'
    category: className
    instVarNames: #()
    comment: '').
  (MCMethodDefinition
    className: className asString
    selector: 'version10Issue156:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'version10Issue156:') asString).
  (MCMethodDefinition
    className: className asString
    selector: 'version11Issue156:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'version11Issue156:') asString).
  (MCMethodDefinition
    className: className asString
    selector: 'version20Issue156:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'version20Issue156:') asString).
  (MCMethodDefinition
    className: className asString
    selector: 'version30Issue156:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'version30Issue156:') asString)}.
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

{ #category : #'Issue 77' }
MetacelloConfigurationResource >> setUpIssue77 [
	"self reset"
	
	self
		setUpIssue77B;
		setUpIssue77C;
		setUpIssue77D
		
]

{ #category : #'Issue 77' }
MetacelloConfigurationResource >> setUpIssue77B [
	"self reset"
	
	| reference className definitionArray |
	reference := GoferVersionReference name: 'MetacelloTestConfigurationOfIssue77B-dkh.1'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: className).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectClassMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline10B:'
						category: 'cat'
						timeStamp: ''
						source: self baseline10MethodSourceB.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MetacelloTestsMCPackage new name: reference packageName)
		info: (MCVersionInfo
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

{ #category : #'Issue 77' }
MetacelloConfigurationResource >> setUpIssue77C [
	"self reset"
	
	| reference className definitionArray |
	reference := GoferVersionReference name: 'MetacelloTestConfigurationOfIssue77C-dkh.1'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: className).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectClassMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline12C:'
						category: 'cat'
						timeStamp: ''
						source: self baseline12MethodSourceC.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline13C:'
						category: 'cat'
						timeStamp: ''
						source: self baseline13MethodSourceC.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MetacelloTestsMCPackage new name: reference packageName)
		info: (MCVersionInfo
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

{ #category : #'Issue 77' }
MetacelloConfigurationResource >> setUpIssue77D [
	"self reset"
	
	| reference className definitionArray |
	reference := GoferVersionReference name: 'MetacelloTestConfigurationOfIssue77D-dkh.1'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: className).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectClassMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline10D:'
						category: 'cat'
						timeStamp: ''
						source: self baseline10MethodSourceD.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MetacelloTestsMCPackage new name: reference packageName)
		info: (MCVersionInfo
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

{ #category : #'Issue 95' }
MetacelloConfigurationResource >> setUpIssue95 [
	"self reset"
	
	self
		setUpConfigurationOfProjectInfinite;
		setUpConfigurationOfProjectLoop;
		setUpConfigurationOfProjectIssue95;
		yourself		
]

{ #category : #running }
MetacelloConfigurationResource >> setUpMonticelloRepository [
	"This method builds a fake repository with the version references from #buildReferences."
	"self reset"

	monticelloRepository := MCDictionaryRepository new.

]

{ #category : #'issue 283' }
MetacelloConfigurationResource >> stableVersion283A: spec [
  "self reset"

  <symbolicVersion: #'stable'>
  spec for: #'common' version: '2.0'
]

{ #category : #'issue 283' }
MetacelloConfigurationResource >> stableVersion283B: spec [
  "self reset"

  <symbolicVersion: #'stable'>
  spec for: #'common' version: '2.1'
]

{ #category : #ConfigurationOfSymbolic }
MetacelloConfigurationResource >> stableVersionC: spec [ 
	"self reset"
	<symbolicVersion: #stable>
	
	spec for: #'common' do: [ spec version: '4.2'].
	spec for: #'platformVersion1.x' do: [ spec version: '4.0'].
	spec for: #'platformVersion1.0.x' do: [ spec version: '4.1'].
	spec for: #'platformVersion1.1.x' do: [ spec version: '4.2'].
	spec for: #'platformVersion1.2.x' do: [ spec version: '4.3'].

]

{ #category : #ConfigurationOfSymbolic }
MetacelloConfigurationResource >> stableVersionD: spec [ 
	"self reset"
	<symbolicVersion: #stable>
	
	spec for: #'common' version: '4.2'.
	spec for: #'platformVersion1.x' version: '4.0'.
	spec for: #'platformVersion1.0.x' version: '4.1'.
	spec for: #'platformVersion1.1.x' version: '4.2'.
	spec for: #'platformVersion1.2.x' version: '4.3'.

]

{ #category : #ConfigurationOfSymbolic }
MetacelloConfigurationResource >> stableVersionDMethodSourceSymbolic [

	^(self class sourceCodeAt: #stableVersionD:) asString


]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> symbolicVersionExplicitlyDoesNotExistProjectToolBox: spec [
    <symbolicVersion: #'explicitly does not exist'>
    spec for: #'common' version: #'notDefined'
]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version100ProjectToolBox: spec [
	<version: '1.0-baseline'>

	spec for: #'common' do: [
		spec blessing: #'testBlessing'.
		spec 
			package: 'Example-Core' with: [
				spec 
					includes: #('Example-AddOn' );
					file: 'Example-Core-anon.1';
					repository: 'http://www.example.com/or';
					preLoadDoIt: #'preloadForCore';
					postLoadDoIt: #'postloadForCore:package:'. ];
			package: 'Example-AddOn' with: [
				spec requires: #('Example-Core' ). ];
			package: 'Example-Tests' with: [
				spec requires: #('Example-AddOn' ). ]. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version101ProjectToolBox: spec [
	<version: '1.0.1-baseline'>

	spec for: #'common' do: [
		spec blessing: #'testBlessing'. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version102ProjectToolBox: spec [
	<version: '1.0.2-baseline'>

	spec for: #'common' do: [
		spec preLoadDoIt: #'preloadForCore'.
		spec postLoadDoIt: #'postloadForCore:package:'.
		spec 
			package: 'Example-Core' with: [
				spec includes: #('Example-AddOn' ). ];
			package: 'Example-AddOn' with: [
				spec requires: #('Example-Core' ). ];
			package: 'Example-Tests' with: [
				spec requires: #('Example-AddOn' ). ]. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version103ProjectToolBox: spec [
	<version: '1.0.3-baseline' imports: #('1.0.2-baseline' )>

	spec for: #'common' do: [
		spec preLoadDoIt: #'alternatePreloadForCore'. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version104ProjectToolBox: spec [
	<version: '1.0.4-baseline' imports: #('1.0.2-baseline' )>

	spec for: #'common' do: [
		spec preLoadDoIt: nil. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version105ProjectToolBox: spec [
	<version: '1.0.5-baseline'>

	spec for: #'common' do: [
		spec package: 'Example-Core' with: [
				spec repository: 'http://www.example.com/ob'. ]. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version106ProjectToolBox: spec [
	<version: '1.0.6-baseline'>

	spec for: #'common' do: [
		spec package: 'Example-Core' with: [
				spec repositories: [
						spec
							repository: 'http://www.example.com/ob';
							repository: 'http://www.example.com/or' ]. ]. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version107ProjectToolBox: spec [
	<version: '1.0.7-baseline'>

	spec for: #'common' do: [
		spec 
			package: 'Example-Core' with: [
				spec 
					supplyingAnswers: #( #( 'preload' 'preload answer' ) #( 'postload' 'postload answer' ) );
					preLoadDoIt: #'preloadForSupplyingAnswers';
					postLoadDoIt: #'postloadForSupplyingAnswers'. ];
			package: 'Example-Tests' with: [
				spec supplyingAnswers: #( #( 'string' 'preload answer' ) #( 'symbol' #'abc def' ) #( 'integer' 1 ) #( 'boolean' true ) #( 'another boolean' false ) #( 'character' $a ) ). ]. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version108ProjectToolBox: spec [
	<version: '1.0.8-baseline'>

	spec for: #'common' do: [
		spec 
			package: 'Example-Core';
			package: 'Example-Tests' with: [
				spec requires: #('Example-Core' ). ]. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version109ProjectToolBox: spec [
	<version: '1.0.9-baseline'>

	spec for: #'common' do: [
		spec 
			package: 'Example-Core';
			package: 'Example-Tests' with: [
				spec includes: #('Example-Core' ). ]. ].

]

{ #category : #'Issue 115' }
MetacelloConfigurationResource >> version10Issue115: spec [
	<version: '1.0' imports: #('1.0-baseline')>

	spec for: #'common' do: [
		spec blessing: #development.
		spec  project: 'Foo' .
		spec package: 'GeauxFoo' with: 'GeauxFoo-lr.1'. ].

]

{ #category : #'Issue 119' }
MetacelloConfigurationResource >> version10Issue119: spec [
    <version: '1.0' imports: #('1.0-baseline')>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec project: 'Foo' with: '2.0'.
            spec package: 'GoferBar' with: 'GoferBar-jf.1' ]
]

{ #category : #'Issue 136' }
MetacelloConfigurationResource >> version10Issue136: spec [
	<version: '1.0' imports: #('1.0-baseline')>

	spec for: #'common' do: [
		spec blessing: #development.
		spec package: 'GoferBeau' with: 'GoferBeau-dkh.15'. ].

]

{ #category : #'Issue 115' }
MetacelloConfigurationResource >> version10Issue156: spec [
  "https://github.com/dalehenrich/metacello-work/issues/156"

  <version: '1.0.0'>
  spec
    for: #'common'
    do: [ 
      spec
        configuration: 'Goo'
          with: [ 
              spec
                versionString: '3.0.0';
                repository:
                    'dictionary://Metacello_MczConfiguration_Test_Repository' ];
        yourself ]
]

{ #category : #'Issue 154' }
MetacelloConfigurationResource >> version10MetacelloExample: spec [
    <version: '1.0' imports: #('1.0-baseline')>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'release'.
            spec package: 'GoferFoo' with: 'GoferFoo-lr.2' ]
]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version110ProjectToolBox: spec [
	<version: '1.1.0-baseline'>

	spec for: #'common' do: [
		spec 
			package: 'Example-Core';
			package: 'Example-Base';
			package: 'Example-Test1' with: [
				spec 
					requires: #('Example-Base' );
					includes: #('Example-Core' ). ];
			package: 'Example-Test2' with: [
				spec 
					includes: #('Example-Core' );
					supplyingAnswers: #( #( 'captionMatch' 'answer' ) ). ];
			package: 'Example-Test3' with: [
				spec 
					requires: #('Example-Base' );
					supplyingAnswers: #( #( 'captionMatch' 'answer' ) ). ];
			package: 'Example-Test4' with: [
				spec 
					requires: #('Example-Base' );
					includes: #('Example-Core' );
					supplyingAnswers: #( #( 'captionMatch' 'answer' ) ). ]. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version111ProjectToolBox: spec [
	<version: '1.1.1-baseline'>

	spec for: #'common' do: [
		spec package: 'Example-Core'.

		spec for: #'nested' do: [
			spec package: 'Example-Base'. ]. ].

	spec for: #'extra' do: [
		spec package: 'Example-Test'. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version112ProjectToolBox: spec [
	<version: '1.1.2-baseline'>

	spec for: #'common' do: [
		spec package: 'Example-Core'.

		spec for: #'nested' do: [
			spec 
				package: 'Example-Base';
				package: 'Example-Test-Nested'. ]. ].

	spec for: #'extra' do: [
		spec package: 'Example-Test'. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version113ProjectToolBox: spec [
	<version: '1.1.3' imports: #('1.1.3-baseline' )>

	spec for: #'outer' do: [
		spec 
			package: 'Example-Core' with: 'Example-Core-dkh.1';
			package: 'Example-Test' with: 'Example-Test-dkh.1'.

		spec for: #'nested' do: [
			spec package: 'Example-Core' with: 'Example-Core-dkh.2'. ]. ].

	spec for: #'extra' do: [
		spec package: 'Example-Core' with: 'Example-Core-dkh.3'. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version114ProjectToolBox: spec [
	<version: '1.1.4-baseline'>

	spec for: #'common' do: [
		spec project: 'Example Project' with: [
				spec
					className: 'MetacelloExampleProjectConfig';
					versionString: '1.0-baseline';
					preLoadDoIt: #'preloadForProject';
					postLoadDoIt: #'postloadForProject';
					repository: 'http://www.example.com/ob' ]. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version115ProjectToolBox: spec [
	<version: '1.1.5-baseline' imports: #('1.1.4-baseline' )>

	spec for: #'common' do: [
		spec project: 'Example Project' with: [
				spec
					preLoadDoIt: #'alternatePreloadForCore' ]. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version116ProjectToolBox: spec [
	<version: '1.1.6-baseline' imports: #('1.1.4-baseline' )>

	spec for: #'common' do: [
		spec project: 'Example Project' with: [
				spec
					preLoadDoIt: #'alternatePreloadForCore' ]. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version117ProjectToolBox: spec [
	<version: '1.1.7-baseline' imports: #('1.0-baseline' )>

	spec for: #'common' do: [
		spec package: 'Example-Core' overrides: [
				spec preLoadDoIt: #'alternatePreloadForCore'. ]. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version118ProjectToolBox: spec [
	<version: '1.1.8-baseline' imports: #('1.0-baseline' )>

	spec for: #'common' do: [
		spec package: 'Example-Core' with: [
				spec preLoadDoIt: #'alternatePreloadForCore'. ]. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version119ProjectToolBox: spec [
	<version: '1.1.9-baseline' imports: #('1.1.4-baseline' )>

	spec for: #'common' do: [
		spec project: 'Example Project' overrides: [
				spec
					className: 'MetacelloExampleProjectConfig';
					versionString: '1.0-baseline';
					preLoadDoIt: #'alternatePreloadForCore';
					repository: 'http://www.example.com/ob' ]. ].

]

{ #category : #'Issue 115' }
MetacelloConfigurationResource >> version11Issue115: spec [
	<version: '1.1' imports: #('1.0-baseline')>

	spec for: #'common' do: [
		spec blessing: #development.
		spec  project: 'Foo' with: ''.
		spec package: 'GeauxFoo' with: 'GeauxFoo-lr.1'. ].

]

{ #category : #'Issue 136' }
MetacelloConfigurationResource >> version11Issue136: spec [
	<version: '1.1' imports: #('1.0-baseline')>

	spec for: #'common' do: [
		spec blessing: #development.
		spec package: 'GoferBeau' with: 'GoferBeau-dkh.25'. ].

]

{ #category : #'Issue 115' }
MetacelloConfigurationResource >> version11Issue156: spec [
  "https://github.com/dalehenrich/metacello-work/issues/156"

  <version: '1.1.0'>
  spec
    for: #'common'
    do: [ 
      spec
        baseline: 'Goo'
          with: [ spec repository: 'dictionary://Metacello_MczConfiguration_Test_Repository' ];
        yourself ]
]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version120ProjectToolBox: spec [
	<version: '1.2.0-baseline' imports: #('1.1.4-baseline' )>

	spec for: #'common' do: [
		spec project: 'Copied Example' copyFrom: 'Example Project' with: [
				spec
					preLoadDoIt: #'alternatePreloadForCore' ]. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version121ProjectToolBox: spec [
	<version: '1.2.1-baseline'>

	spec for: #'common' do: [
		spec blessing: #'testBlessing'.
		spec repository: 'http://www.example.com/vor'.
		spec 
			package: 'Example-Core' with: [
				spec 
					includes: #('Example-AddOn' );
					file: 'Example-Core-anon.1';
					repository: 'http://www.example.com/or';
					preLoadDoIt: #'preloadForCore';
					postLoadDoIt: #'postloadForCore:package:'. ];
			package: 'Example-AddOn' with: [
				spec requires: #('Example-Core' ). ];
			package: 'Example-Tests' with: [
				spec requires: #('Example-AddOn' ). ]. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version122ProjectToolBox: spec [
	<version: '1.2.2-baseline'>

	spec for: #'common' do: [
		spec project: 'Example Project' with: [
				spec
					className: 'MetacelloExampleProjectConfig';
					repository: 'http://www.example.com/ob' ]. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version123ProjectToolBox: spec [
	<version: '1.2.3-baseline'>

	spec for: #'common' do: [
		spec project: 'Example Project' with: [
				spec
					className: 'MetacelloExampleProjectConfig';
					repository: 'http://www.example.com/ob';
					repository: 'http://www.example.com/or' ]. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version124ProjectToolBox: spec [
	<version: '1.2.4' imports: #('1.2.4-baseline' )>

	spec for: #'common' do: [
		spec repository: 'http://www.example.com/ac'. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version125ProjectToolBox: spec [
	<version: '1.2.5-baseline' imports: #('1.0-baseline' )>

	spec for: #'common' do: [
		spec package: 'Example-Core' with: [
				spec 
					preLoadDoIt: #'preloadDoIt';
					postLoadDoIt: #'postloadDoIt'. ]. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version126ProjectToolBox: spec [
	<version: '1.2.6-baseline' imports: #('1.2.5-baseline' )>

	spec for: #'common' do: [
		spec package: 'Example-Core' with: [
				spec 
					preLoadDoIt: nil;
					postLoadDoIt: nil. ]. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version127ProjectToolBox: spec [
	<version: '1.2.7-baseline'>

	spec for: #'common' do: [
		spec project: 'Example Project' with: [
				spec
					className: 'MetacelloExampleProjectConfig';
					versionString: '1.0-baseline';
					preLoadDoIt: #'preloadDoIt';
					postLoadDoIt: #'postloadDoIt';
					repository: 'http://www.example.com/ob' ]. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version128ProjectToolBox: spec [
	<version: '1.2.8-baseline' imports: #('1.2.7-baseline' )>

	spec for: #'common' do: [
		spec project: 'Example Project' with: [
				spec
					preLoadDoIt: nil;
					postLoadDoIt: nil ]. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version129ProjectToolBox: spec [
	<version: '1.2.9-baseline'>

	spec for: #'common' do: [
		spec 
			project: 'Example Project' with: [
				spec
					className: 'MetacelloExampleProjectConfig';
					versionString: '1.0-baseline';
					preLoadDoIt: #'preloadDoIt';
					postLoadDoIt: #'postloadDoIt';
					repository: 'http://www.example.com/ob' ];
			project: 'Copy Project' copyFrom: 'Example Project' with: [
				spec
					preLoadDoIt: nil;
					postLoadDoIt: nil ]. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version130ProjectToolBox: spec [
	<version: '1.3.0-baseline'>

	spec for: #'common' do: [
		spec 
			project: 'Example Project' with: [
				spec
					className: 'MetacelloExampleProjectConfig';
					versionString: '1.0-baseline';
					preLoadDoIt: #'preloadDoIt';
					postLoadDoIt: #'postloadDoIt';
					repository: 'http://www.example.com/ob' ];
			project: 'Copy Project' copyFrom: 'Example Project' with: [
				spec repository: 'http://www.example.com/or' ]. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version131ProjectToolBox: spec [
	<version: '1.3.1-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'. ].

	spec for: #'level1_1' do: [
		spec 
			package: 'Example-Core' with: [
				spec repository: 'http://www.example.com/1'. ];
			package: 'Example-Core' with: [
				spec repository: 'http://www.example.com/2'. ];
			package: 'Example-Core' with: [
				spec repository: 'http://www.example.com/3'. ].

		spec for: #'level2_1' do: [
			spec package: 'Example-Core' with: [
					spec repository: 'http://www.example.com/4'. ].

			spec for: #'level3_1' do: [
				spec package: 'Example-Core' with: [
						spec repository: 'http://www.example.com/5'. ]. ]. ].

		spec for: #'level2_2' do: [
			spec package: 'Example-Core' with: [
					spec repository: 'http://www.example.com/6'. ].

			spec for: #'level3_2' do: [
				spec package: 'Example-Core' with: [
						spec repository: 'http://www.example.com/7'. ]. ]. ]. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version132ProjectToolBox: spec [
	<version: '1.3.2-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'. ].

	spec for: #'level1_1' do: [
		spec package: 'GoferBeau' with: 'GoferBeau-dkh.15'.

		spec for: #'level2_1' do: [
			spec package: 'GoferBeau' with: 'GoferBeau-dkh.25'.

			spec for: #'level3_1' do: [
				spec package: 'GoferBeau' with: 'GoferBeau-dkh.53'. ]. ].

		spec for: #'level2_2' do: [
			spec package: 'GoferBeau' with: 'GoferBeau-dkh.54'.

			spec for: #'level3_2' do: [
				spec package: 'GoferBeau' with: 'GoferBeau-dkh.55'. ]. ]. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version133ProjectToolBox: spec [
	<version: '1.3.3-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'. ].

	spec for: #'level1_1' do: [
		spec package: 'GoferBar' with: 'GoferBar-jf.1'.

		spec for: #'level2_1' do: [
			spec package: 'GoferFoo' with: 'GoferFoo-lr.1'.

			spec for: #'level3_1' do: [
				spec package: 'GoferFaux' with: 'GoferFaux-tg.30'. ]. ].

		spec for: #'level2_2' do: [
			spec package: 'GeauxBar' with: 'GeauxBar.branch-lr.2'.

			spec for: #'level3_2' do: [
				spec package: 'GeauxBeau' with: 'GeauxBeau-dkh.55'. ]. ]. ].

	spec for: #'pharo' do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'. ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version134ProjectToolBox: spec [
	<version: '1.3.4-baseline'>

	spec for: #'common' do: [
 ].

]

{ #category : #ConfigurationOfProjectToolBox }
MetacelloConfigurationResource >> version135ProjectToolBox: spec [
	<version: '1.3.5' imports: #('1.2.4-baseline' )>

	spec for: #'common' do: [
		spec repositories: [
			spec
				repository: 'http://www.example.com/ab';
				repository: 'http://www.example.com/ac' ]. ].

]

{ #category : #'Issue 119' }
MetacelloConfigurationResource >> version20Issue119: spec [
    <version: '2.0' imports: #('2.0-baseline')>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec project: 'Foo' with: '2.0'.
            spec package: 'GoferBar' with: 'GoferBar-jf.1' ]
]

{ #category : #'Issue 115' }
MetacelloConfigurationResource >> version20Issue156: spec [
  "https://github.com/dalehenrich/metacello-work/issues/156"

  <version: '2.0.0'>
  spec
    for: #'common'
    do: [ 
      spec
        configuration: 'Goo'
          with: [ 
              spec
                versionString: '4.0.0';
                repository:
                    'dictionary://Metacello_MczConfiguration_Test_Repository' ];
        yourself ]
]

{ #category : #'Issue 171' }
MetacelloConfigurationResource >> version20Issue171: spec [
    <version: '2.0' imports: #('2.0-baseline')>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec project: 'Foo' with: '2.0'.
            spec package: 'GeauxFoo' with: 'GeauxFoo-lr.1' ]
]

{ #category : #'issue 283' }
MetacelloConfigurationResource >> version20Issue283: spec [
  <version: '2.0'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'release'.
      spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
      spec package: 'GoferFoo' with: 'GoferFoo-lr.2' ]
]

{ #category : #'Issue 171' }
MetacelloConfigurationResource >> version21Issue171: spec [
    <version: '2.1' imports: #('2.1-baseline')>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec project: 'Foo' with: '2.0'.
            spec package: 'GeauxFoo' with: 'GeauxFoo-lr.1' ]
]

{ #category : #'issue 283' }
MetacelloConfigurationResource >> version21Issue283: spec [
  <version: '2.1'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'release'.
      spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
      spec package: 'GoferFoo' with: 'GoferFoo-lr.4' ]
]

{ #category : #'Issue 119' }
MetacelloConfigurationResource >> version30Issue119: spec [
    <version: '3.0' imports: #('3.0-baseline')>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec project: 'Foo' with: '2.0'.
            spec package: 'GoferBar' with: 'GoferBar-jf.1' ]
]

{ #category : #'Issue 125' }
MetacelloConfigurationResource >> version30Issue125: spec [
    <version: '3.0' imports: #('3.0-baseline')>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'release'.
            spec description: 'MetacelloConfigurationResource>>version30Issue125:'.
            spec
                package: 'GoferFoo' with: 'GoferFoo-lr.4';
                package: 'GoferBar' with: 'GoferBar-jf.1';
                yourself ].
    spec
        for: #'attribute1'
        do: [ 
            spec
                package: 'GoferFaux' with: 'GoferFaux-tg.32';
                package: 'GoferBeau' with: 'GoferBeau-dkh.55';
                yourself ].
    spec
        for: #'attribute2'
        do: [ 
            spec
                package: 'GoferFaux' with: 'GoferFaux-tg.33';
                package: 'GoferBeau' with: 'GoferBeau-dkh.56';
                yourself ]
]

{ #category : #'Issue 115' }
MetacelloConfigurationResource >> version30Issue156: spec [
  "https://github.com/dalehenrich/metacello-work/issues/156"

  <version: '3.0.0'>
  spec
    for: #'common'
    do: [ 
      spec
        configuration: 'Goo'
          with: [ 
              spec
                versionString: '5.0.0';
                repository:
                    'dictionary://Metacello_MczConfiguration_Test_Repository' ];
        yourself ]
]

{ #category : #'Issue 119' }
MetacelloConfigurationResource >> version40Issue119: spec [
    <version: '4.0' imports: #('4.0-baseline')>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec project: 'Foo' with: '2.0'.
            spec package: 'GoferBar' with: 'GoferBar-jf.1' ]
]

{ #category : #ConfigurationOfSymbolic }
MetacelloConfigurationResource >> version40Symbolic: spec [ 
	"self reset"
	<version: '4.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec blessing: #release.
		spec 
			package: 'GoferBeau' with: 'GoferBeau-dkh.15';
			package: 'GoferFaux' with:'GoferFaux-tg.30';
			yourself ].

]

{ #category : #ConfigurationOfSymbolic }
MetacelloConfigurationResource >> version40SymbolicMethodSourceSymbolic [

	^(self class sourceCodeAt: #version40Symbolic:) asString


]

{ #category : #'Issue 119' }
MetacelloConfigurationResource >> version41Issue119: spec [
    <version: '4.1' imports: #('4.0-baseline')>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec project: 'Foo ' with: '2.0'.
            spec package: 'GoferBar' with: 'GoferBar-jf.1' ]
]

{ #category : #ConfigurationOfSymbolic }
MetacelloConfigurationResource >> version41Symbolic: spec [ 
	"self reset"
	<version: '4.1'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec blessing: #release.
		spec 
			package: 'GoferBeau' with: 'GoferBeau-dkh.25';
			package: 'GoferFaux' with:'GoferFaux-tg.31';
			yourself ].

]

{ #category : #ConfigurationOfSymbolic }
MetacelloConfigurationResource >> version41SymbolicMethodSourceSymbolic [

	^(self class sourceCodeAt: #version41Symbolic:) asString


]

{ #category : #'Issue 119' }
MetacelloConfigurationResource >> version42Issue119: spec [
    <version: '4.2' imports: #('4.0-baseline')>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec project: 'Foo' with: '2.0'.
            spec package: 'GoferBar ' with: 'GoferBar-jf.1' ]
]

{ #category : #ConfigurationOfSymbolic }
MetacelloConfigurationResource >> version42Symbolic: spec [ 
	"self reset"
	<version: '4.2'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec blessing: #release.
		spec 
			package: 'GoferBeau' with: 'GoferBeau-dkh.53';
			package: 'GoferFaux' with:'GoferFaux-tg.32';
			yourself ].

]

{ #category : #ConfigurationOfSymbolic }
MetacelloConfigurationResource >> version42SymbolicMethodSourceSymbolic [

	^(self class sourceCodeAt: #version42Symbolic:) asString


]

{ #category : #'Issue 119' }
MetacelloConfigurationResource >> version43Issue119: spec [
    <version: '4.3' imports: #('4.0-baseline')>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec project: 'Foo ' with: '2.0'.
            spec package: 'GoferBar ' with: 'GoferBar-jf.1' ]
]

{ #category : #ConfigurationOfSymbolic }
MetacelloConfigurationResource >> version43Symbolic: spec [ 
	"self reset"
	<version: '4.3'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec blessing: #development.
		spec 
			package: 'GoferBeau' with: 'GoferBeau-dkh.54';
			package: 'GoferFaux' with:'GoferFaux-tg.33';
			yourself ].

]

{ #category : #ConfigurationOfSymbolic }
MetacelloConfigurationResource >> version43SymbolicMethodSourceSymbolic [

	^(self class sourceCodeAt: #version43Symbolic:) asString


]

{ #category : #'Issue 119' }
MetacelloConfigurationResource >> version44Issue119: spec [
    <version: '4.4' imports: #('4.0-baseline')>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec project: 'Foo' with: 2.0.
            spec package: 'GoferBar ' with: 'GoferBar-jf.1' ]
]

{ #category : #'Issue 119' }
MetacelloConfigurationResource >> version50Issue119: spec [
    <version: '5.0' imports: #('4.0-baseline')>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec project: 'Foo' with: '1.0'.
            spec package: 'GoferBar' with: 'GoferBar-jf.1' ]
]
