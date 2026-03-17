let rec howMany e l =
    if l = [] then                              *base case*
        0
    else if e = List.hd l then
        1 + howMany e (List.tl l)               *first recursive case*
    else
        howMany e (List.tl l)                   *second recursive case*

let rec delete e l =
    if l = [] then                              *base case*
        []
    else if e = List.hd l then
        delete e (List.tl l)                    *first recursive case*
    else
        (List.hd l) :: delete e (List.tl l)     *second recursive case*

let mean l = 
    let rec sum lst =                           *helper sum function*
        if lst = [] then                        *helper sum base case*
            0.0
        else
            List.hd lst +. sum (List.tl lst)    *helper recursive case*
    in
    let rec length lst =                        *helper length function*
        if lst = [] then                        *helper base case*
            0.0
        else
            1 + length (List.tl lst)            *helper recursive case*
    in
    sum l /. length l