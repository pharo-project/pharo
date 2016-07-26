I represent a model of an open sequence of connected points that can be queried for
enclosing bounds and whether a point lies along any segment.
 
I am typically used for drawing with a Canvas.

ps := PathShape new
        addVertex: 0@0;
        addVertex: 30@30;
        addVertex: 50@10.
        
self assert: (ps containsPoint: 24@24).
self assert: (ps containsPoint: 40@30) not.
Transcript show: ps calculatedBounds asString; cr.