tests-Getting Started
testFileShortcuts

| materializedString |
FLSerializer serialize: 'stringToSerialize' toFileNamed: 'demo.fuel'. 
materializedString := FLMaterializer materializeFromFileNamed: 'demo.fuel'.
