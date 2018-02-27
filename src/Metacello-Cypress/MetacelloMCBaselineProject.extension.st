Extension { #name : #MetacelloMCBaselineProject }

{ #category : #'*metacello-cypress' }
MetacelloMCBaselineProject >> packageSpecClass [
  "One could argue that Baselines should have done this from the very beginning ... the cost is that every package is fetched (from disk) and the snapshots for every package are created ... the advantage is that you will properly downgrade packages when switching git versions ... for now a class variable is sufficient"

  MetacelloCypressBaselineProject useCypressPackagesForAllBaselines
    ifTrue: [ ^ MetacelloCypressPackageSpec ].
  ^ super packageSpecClass
]
