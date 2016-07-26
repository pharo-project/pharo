i represent a quadric Bezier path segment.

Any quadratic spline can be expressed as a cubic (where the cubic term is zero). The end points of the cubic will be the same as the quadratic's.

    CP0 = QP0
    CP3 = QP2 

The two control points for the cubic are:

    CP1 = QP0 + 2/3 *(QP1-QP0)
    CP2 = QP2 + 2/3 *(QP1-QP2) 