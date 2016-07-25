tests
testNilExtensionOnDraw

	| morph |

	morph := Morph new.

	self assert: morph hasExtension not.

	morph imageForm.

	self assert: morph hasExtension not.