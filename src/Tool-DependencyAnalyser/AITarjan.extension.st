Extension { #name : #AITarjan }

{ #category : #'*Tool-DependencyAnalyser' }
AITarjan >> runOnDAPackageRelationGraph: aRelationGraph [

    | dependencies |
    "Construct the internal structure of Tarjan from a DAPackageRelation"
    dependencies := (aRelationGraph packages collect: [ :each | each dependencies ]) flattened.

    self nodes: aRelationGraph packages.
    self
        edges: dependencies
        from: [ :each | each source ]
        to: [ :each | each target ].

    self run.

    "Convert the strongly connected components to a DAPackageRelation"
    ^ self stronglyConnectedComponents collect: [ :packages |
        DAPackageRelationGraph onPackages: packages ]
]
