miscellaneous
metacelloPlatformAttributes
    "Returns the tags for the conditional platform loading in Metacello. Pay attention the order is important: from most  to least general." 
    "For release integrators, we should not have #'pharo1.3x' **and** #'pharo1.4.x'"
   ^ #(#squeakCommon #pharo #'pharo6.x' #'pharo6.0.x')