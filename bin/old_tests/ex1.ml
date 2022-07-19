type 'a t = {capacity: int; contents: 'a list}

exception Silly

let create silly c = ({capacity = c; contents = []}, silly)
