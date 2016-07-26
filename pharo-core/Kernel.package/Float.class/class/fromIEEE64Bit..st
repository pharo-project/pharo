instance creation
fromIEEE64Bit: doubleWord

    ^ (self new: 2) basicAt: 1 put: (doubleWord bitShift: -32); basicAt: 2 put: (doubleWord bitAnd: 16rFFFFFFFF); yourself