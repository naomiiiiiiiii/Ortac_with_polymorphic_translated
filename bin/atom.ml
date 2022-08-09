type t = int Atomic.t

let init_sut () = Atomic.make 42

let get = Atomic.get

let set = Atomic.set

let exchange = Atomic.exchange

let compare_and_set = Atomic.compare_and_set

let five_args _atom _one _two _three _four _five = true 

