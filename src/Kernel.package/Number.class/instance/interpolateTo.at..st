mathematical functions
interpolateTo: aNumber at: param
    ^  self * (1 - param) + (aNumber * param)