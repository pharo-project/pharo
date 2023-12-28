Extension { #name : 'BlockClosure' }

{ #category : '*Metacello-Core' }
BlockClosure >> execute: projectSpecBlock against: aScriptExecutor [
    aScriptExecutor executeBlock: self do: projectSpecBlock
]
