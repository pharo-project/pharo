copying
blankCopyOf: aRectangle scaledBy: scale

        ^ self class extent: (aRectangle extent * scale) truncated depth: depth