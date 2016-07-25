tests-keyboard
testSmartNeedExtraRemoveInForOppositeAt

	self assert: (controller smartNeedExtraRemoveIn: '()' for: $( opposite:$) at:1).
	self deny: (controller smartNeedExtraRemoveIn: '(()' for: $( opposite:$) at:2).
	self assert: (controller smartNeedExtraRemoveIn: '(1()' for: $( opposite:$) at:3).
	self assert: (controller smartNeedExtraRemoveIn: '(1(1())' for: $( opposite:$) at:5).
	self assert: (controller smartNeedExtraRemoveIn: '((1)1())))' for: $( opposite:$) at:6).
	self deny: (controller smartNeedExtraRemoveIn: '()(()' for: $( opposite:$) at:3).
	self assert: (controller smartNeedExtraRemoveIn: '(foobar()' for: $( opposite:$) at:8).