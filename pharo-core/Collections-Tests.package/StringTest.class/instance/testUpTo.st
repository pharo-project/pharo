tests - converting
testUpTo
	"self debug: #testUpTo"
	self assert: #up:to: keywords = #(up: to:).
	self assert: #copy:from:to: keywords = #(copy: from: to:).
	self assert: #up keywords = #(up).
	self assert: #at: keywords = #(at:).
	