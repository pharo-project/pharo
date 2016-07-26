private
image: aForm at: aPoint sourceRect: sourceRect rule: rule
 
    self flag: #MayBeWrong.
    self canvas pathTransform
        restoreAfter: [ 
            self canvas pathTransform translateBy: aPoint.
            self canvas setPaint: aForm.
            self canvas drawShape: (0 @ 0 extent: sourceRect extent) ] 