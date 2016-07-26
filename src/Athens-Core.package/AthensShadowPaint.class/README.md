I representing a shadow paint object used by Athens.
My subclasses provide backend-specific implementation.
I am more serving to define the common protocols and
requirements for all backends (such as defaults).


IMPORTANT NOTE: a shadow paint object provides a protocol 
only for setting shadow properties, but not retrieving them back.
This is intentionally, because the way how these properties is
managed are highly backend-specific. 

The shadow paint has following properties:
 - color. A shadow color defined in RGBA color space.

	Default: Color black alpha: 0.5

 - shadow width. Can be set using #width:

	Default: if width is not set explicitly for shadow paint,
		it is assumed to be equal to 2.0.
		
 - shadow offset. Defines how far shadow is located.
	Shadow offset is a point where x component defines ofset on x-asix and y component on y-axis.
	
	Default: if offset is not set  explicitely it is assumed to be equal 2@2.
			
 - shadow blur. Defines how smooth shadow is.
	Blur is a point with x component defining bluring along x-axis and y component along y axis
	
	Default: if blur is not set explicitely it is assumed to be equal 4@4
