I represent an x-y pair of numbers usually designating a location on the screen.

My instances are created either using the message @ or x:y: or r:degrees: as follows:

[[[
| pt |
pt := 10@20.
pt x 
> 10
pt y
> 20 			 
]]]

[[[
| pt |
pt := Point x: 10 y: 20.
pt x 
> 10
pt y
> 20 			 
]]]

I define many nice messages that deal with point such as: 
- arithmetic such as +, *, reciprocal, min, abs,
- comparison <, <=, =, >, >=, closeTo: 
- geometry such as sideOf:, to:intersects:to:, 
- polar coordinates,
- extent such as scaleTo:
- transformation such as negated, translatedBy:, scaleBy:
- rounding with roundTo:, roundUpTo:, truncateTo:, truncated

