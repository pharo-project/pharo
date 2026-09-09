Extension { #name : 'ClapContext' }

{ #category : '*Clap-UI' }
ClapContext >> inspectGraph: aBuilder [

    <inspectorPresentationOrder: 950 title: 'Match graph'>

    ^ (aBuilder instantiate: SpRoassalInspectorPresenter)
          canvas: (ClapMatchGraphBuilder forContext: self) build;
          yourself
]
