bordering
border: rect width: borderWidth rule: rule fillColor: fillColor
        "Paint a border whose rectangular area is defined by rect. The
width of the border of each side is borderWidth. Uses fillColor for drawing
the border."
        | blt |
        blt := (BitBlt toForm: self) combinationRule: rule; fillColor: fillColor.
        blt sourceOrigin: 0@0.
        blt destOrigin: rect origin.
        blt width: rect width; height: borderWidth; copyBits.
        blt destY: rect corner y - borderWidth; copyBits.
        blt destY: rect origin y + borderWidth.
        blt height: rect height - borderWidth - borderWidth; width:
borderWidth; copyBits.
        blt destX: rect corner x - borderWidth; copyBits