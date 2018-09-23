Extension { #name : #BlockClosuresTestCase }

{ #category : #'*Kernel-Tests-Extended' }
BlockClosuresTestCase >> testExample1 [

   self assert: ((self example1: 5) = 5 factorial)
  
]
