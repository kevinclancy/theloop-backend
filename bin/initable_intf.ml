
module type Initable = sig
  (** TESTING TESTING TESTING *)
  val init : Caqti_lwt.connection -> (unit, string) Lwt_result.t
end