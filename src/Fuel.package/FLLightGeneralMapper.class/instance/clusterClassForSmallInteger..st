protected-mapping
clusterClassForSmallInteger: aSmallInteger

	aSmallInteger >= 0 ifTrue: [
		aSmallInteger <= 255 ifTrue: [^ FLPositive8SmallIntegerCluster].
		aSmallInteger <= 65535 ifTrue: [^ FLPositive16SmallIntegerCluster].
		aSmallInteger <= 16777215 ifTrue: [^ FLPositive24SmallIntegerCluster].
		aSmallInteger <= 4294967295 ifTrue: [^ FLPositive32SmallIntegerCluster].
	].
	aSmallInteger >= -128 ifTrue: [^ FLNegative8SmallIntegerCluster].
	aSmallInteger >= -32768 ifTrue: [^ FLNegative16SmallIntegerCluster].
	aSmallInteger >= -8388608 ifTrue: [^ FLNegative24SmallIntegerCluster].
	aSmallInteger >= -2147483648 ifTrue: [^ FLNegative32SmallIntegerCluster].

