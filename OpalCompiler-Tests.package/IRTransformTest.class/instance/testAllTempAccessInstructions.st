testing - enumeration
testAllTempAccessInstructions
		| ir |

		ir := IRBuilderTest new testPushTempTemp.
		self assert: ir allTempAccessInstructions size = 1.