private
emphasisDepractedFor: aCode
	^ 'Using numbers for emphasis is deprecated, use symbols instead.
Instead of ' , aCode asString , ' use ' , (self emphasisForCode: aCode) asString , '.'