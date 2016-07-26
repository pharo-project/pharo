I am FloatPrintPolicy.
I am a DynamicVariable.

I control whether Float instances are printed exactly or inexactly. The inexact printing is much faster, but can be less accurate. 

The default policy is ExactFloatPrintPolicy.

FloatPrintPolicy 
	value: InexactFloatPrintPolicy new 
	during: [ Float pi printString ]