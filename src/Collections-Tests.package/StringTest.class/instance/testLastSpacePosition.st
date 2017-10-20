tests
testLastSpacePosition

self assert: ('fred the bear' lastSpacePosition) = 9.
self assert: ('ziggie' lastSpacePosition) = 0.
self assert: ('elvis ' lastSpacePosition) = 6.
self assert: ('wimpy  ' lastSpacePosition) = 7.
self assert: ('' lastSpacePosition) = 0.
self assert: (' 123  éà ôüÖ ẞ' lastSpacePosition) = 13.