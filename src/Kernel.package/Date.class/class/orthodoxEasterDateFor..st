specific inquiries
orthodoxEasterDateFor: year

 "  compute the easter date according to the rules of the orthodox calendar.
    source: 
    http://www.smart.net/~mmontes/ortheast.html 
  "
     | r1 r2 r3 r4 ra rb r5 rc date |

    r1 := year \\ 19.
    r2 := year \\ 4.
    r3 := year \\ 7.
    ra := 19*r1 + 16.
    r4 := ra \\ 30.
    rb := r2 + r2 + (4*r3) + (6*r4).
    r5 := rb \\ 7.
    rc := r4 + r5.
    date := Date year: year month: 4 day: 3.
    ^date addDays: rc.