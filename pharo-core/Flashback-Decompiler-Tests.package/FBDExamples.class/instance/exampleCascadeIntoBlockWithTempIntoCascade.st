examples - cascades
exampleCascadeIntoBlockWithTempIntoCascade
	| t |
	t := {{true}.
	{true}.
	{false}}.
	^ t
		size;
		collect: [ :aBooleanArray | 
					aBooleanArray
						size;
						collect: [ :aBoolean | 
									| aSecondBoolean aThirdBoolean |
									aSecondBoolean := true.
									aThirdBoolean := false.
									aBoolean
										not;
										or: [ aSecondBoolean ];
										and: [ aThirdBoolean ];
										value ] ]