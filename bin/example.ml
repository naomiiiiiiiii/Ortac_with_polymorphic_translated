type 'a t = {capacity: int; contents: 'a list}

let silly_create c = {capacity = c; contents = []}
