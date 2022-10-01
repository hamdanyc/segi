#     # Run whenever save button is pressed
#     # Print the objects being saved
#     # print(rls())
#     # Put  objects into current environment
#     for(obj in unlist(rls())) {
#         if(class(get(obj, pos =  -1))[1] == "reactive"){
#             ## execute the reactive objects and put them in to this
#             ## environment i.e. into the environment of this function
#             assign(obj, value = eval(call(obj)))
#         } else {
#             ## grab the global variables and put them into this
#             ## environment
#             assign(obj, value = get(obj, pos =  -1))
#         }
#     }
#     
#     input_copy <- list()
#     for(nm in names(input)){
#         # assign(paste0("input_copy$", nm), value <- input[[nm]])
#         input_copy[[nm]] <- input[[nm]]
#     }
#     
#     ## save objects in current environment
#     save(list = ls(), file = "cuti.RData", envir = environment())
#     
#     # print("** done saving     **")
# })