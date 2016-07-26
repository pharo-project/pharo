conversions
contrastingForegroundColor
        "Make a foreground color contrasting with me"
        ^self luminance >= 0.5
                ifTrue: [self class black]
                ifFalse: [self class white]