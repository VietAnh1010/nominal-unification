module Options = struct
  let ( let* ) = Option.bind
end

module Results = struct
  let ( let* ) = Result.bind
end
