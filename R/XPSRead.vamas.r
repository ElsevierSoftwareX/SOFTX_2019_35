#routine to read vamas filedata 
#'
#'read.vamas
#'
#'Read vamas format
#'
#'Read vamas format
#'
#'@param file File
#'@param debug debug switch
#'
#'
#'


read.vamas <- function(file = NULL, debug = FALSE) {


myreadLines <- function( fp, n, debug = FALSE ) {
	tmp <- readLines( fp, n=n )
	if (debug) cat(tmp,"\n")
	return(tmp)
}


read_block <- function(fp, include, experiment_mode, exp_variables, scan_mode, block_future_upgrade_entries) {
    block_identifier <- myreadLines( fp, n=1 )
	#cat("block_identifier ",block_identifier,"\n")
	sample_identifier <- myreadLines( fp, n=1 )
	if ( include[1] ) year <- myreadLines( fp, n=1 )
	if ( include[2] ) month <- myreadLines( fp, n=1 )
	if ( include[3] ) day <- myreadLines( fp, n=1 )
	if ( include[4] ) hours <- myreadLines( fp, n=1 )
	if ( include[5] ) minutes <- myreadLines( fp, n=1 )
	if ( include[6] ) seconds <- myreadLines( fp, n=1 )
	if ( include[7] ) hours_adv_GMT <- myreadLines( fp, n=1 )
	if ( include[8] ) {
		num_lines_in_block_comment <- myreadLines( fp, n=1 )
		if (num_lines_in_block_comment > 0) {
			block_comments_lines <- myreadLines( fp, n=num_lines_in_block_comment )
			}
		}
	if ( include[9] ) {
		technique <- myreadLines( fp, n=1 )
		#cat("technique ",technique,"\n")
		technique <- match.arg(technique, c("AES diff","AES dir","EDX","ELS","FABMS","FABMS energy spec","ISS","SIMS","SIMS energy spec", "SNMS", "SNMS energy spec", "UPS", "XPS", "XRF" ))
		}

	if ( include[10] ) {
		if ( experiment_mode %in% c("MAP","MAPDP") ) {
			x_coord <- myreadLines( fp, n=1 )
			y_coord <- myreadLines( fp, n=1 )
			}
		}
	if ( include[11] && exp_variables > 0 ) { exp_variables_values <- myreadLines( fp, n=exp_variables ) }
	if ( include[12] ) analysis_source_label <- myreadLines( fp, n=1 )
	if ( include[13] ) {
		if ( ( experiment_mode %in% c("MAPDP","MAPSVDP","SDP","SDPSV") ) || (technique %in% c("FABMS","FABMS energy spec","ISS","SIMS","SIMS energy spec", "SNMS", "SNMS energy spec")) ) {
			atomic_number <- myreadLines( fp, n=1 )
			atom_charge <- myreadLines( fp, n=1 )
			}
	}
	if ( include[14] ) { analysis_source_energy <- myreadLines( fp, n=1 ) }
	if ( include[15] ) { analysis_source_strength <- myreadLines( fp, n=1 ) }
	if ( include[16] ) { 
		analysis_source_beamwidth_x <- myreadLines( fp, n=1 ) 
		analysis_source_beamwidth_y <- myreadLines( fp, n=1 )
		}
	if ( include[17] ) { 
		if ( experiment_mode %in% c("MAP","MAPDP","MAPSV","MAPSVDP","SEM") ) {
			field_view_x <- myreadLines( fp, n=1 ) 
			field_view_y <- myreadLines( fp, n=1 )
		}
	}
	if ( include[18] ) { 
		if ( experiment_mode %in% c("MAPSV","MAPSVDP","SEM") ) {
			first_linescan_start_x <- myreadLines( fp, n=1 ) 
			first_linescan_start_y <- myreadLines( fp, n=1 )
			first_linescan_finish_x <- myreadLines( fp, n=1 )
			first_linescan_finish_y <- myreadLines( fp, n=1 )
			last_linescan_finish_x <- myreadLines( fp, n=1 )
			last_linescan_finish_y <- myreadLines( fp, n=1 )
		}
	}
	if ( include[19] ) { analysis_source_polar_angle <- myreadLines( fp, n=1 ) }
	if ( include[20] ) { analysis_source_azimuth <- myreadLines( fp, n=1 ) }
	if ( include[21] ) { analyser_mode <- myreadLines( fp, n=1 ) }
	#cat("analyser_mode ",analyser_mode)

	if ( include[22] ) { analyser_pass_energy <- myreadLines( fp, n=1 ) }
	if ( include[23] && technique == "AES diff" ) { differential_width <- myreadLines( fp, n=1 ) }
	if ( include[24] ) { magnification_transfer_lens <- myreadLines( fp, n=1 ) }
	if ( include[25] ) { analyser_work_function <- myreadLines( fp, n=1 ) }
	if ( include[26] ) { target_bias <- myreadLines( fp, n=1 ) }
	if ( include[27] ) { 
		analysis_width_x <- myreadLines( fp, n=1 ) 
		analysis_width_y <- myreadLines( fp, n=1 )
		}
	if ( include[28] ) { 
		axis_take_off_polar <- myreadLines( fp, n=1 ) 
		axis_take_off_azimuth <- myreadLines( fp, n=1 ) 
		}
	if ( include[29] ) { special_label <- myreadLines( fp, n=1 ) }
	if ( include[30] ) { 
		charge_state_label <- myreadLines( fp, n=1 ) 
		charge_particle <- myreadLines( fp, n=1 )
		}
	if ( include[31] ) { 
		if (scan_mode == "REGULAR") {
			abscissa_label <- myreadLines( fp, n=1 )
			abscissa_unit <- myreadLines( fp, n=1 )
			abscissa_start <- myreadLines( fp, n=1 )
			abscissa_increment <- myreadLines( fp, n=1 )
			}
		}
	if ( include[32] ) { 
		corresponding_variables_num <- myreadLines( fp, n=1 )
		corresponding_variables <- list() 
		for (var in 1:corresponding_variables_num) {
			corresponding_variables[[var]] <- list(label=myreadLines( fp, n=1 ),
													units=myreadLines( fp, n=1 ))
			}
		}
	if ( include[33] ) { signal_mode <- myreadLines( fp, n=1 ) }
	if ( include[34] ) { signal_collection_time <- myreadLines( fp, n=1 ) }
	if ( include[35] ) { num_scans_block <- myreadLines( fp, n=1 ) }
	if ( include[36] ) { signal_time_correction <- myreadLines( fp, n=1 ) }
	if ( include[37] ) {
		if ( ( experiment_mode %in% c("MAPDP","MAPSVDP","SDP","SDPSV") ) && (technique %in% c("AES diff","AES dir","EDX","ELS","UPS", "XPS", "XRF") ) ) {
			sputtering_source_energy <- myreadLines( fp, n=1 )
			sputtering_source_beam_current <- myreadLines( fp, n=1 )
			sputtering_source_width_x <- myreadLines( fp, n=1 )
			sputtering_source_width_y <- myreadLines( fp, n=1 )
			sputtering_source_polar_angle <- myreadLines( fp, n=1 )
			sputtering_source_azimuth <- myreadLines( fp, n=1 )
			sputtering_mode <- myreadLines( fp, n=1 )
			}
	}
	if ( include[38] ) { 
		sample_tilt_angle <- myreadLines( fp, n=1 ) 
		sample_tilt_azimuth <- myreadLines( fp, n=1 )
		}
	if ( include[39] ) { sample_rotation_angle <- myreadLines( fp, n=1 ) }
	if ( include[40] ) { 
		num_additional_parameters <- myreadLines( fp, n=1 ) 
		if ( num_additional_parameters > 0 ) {
			for (var in 1:num_additional_parameters) {
				additional_parameters_labels <- myreadLines( fp, n=1 )
				additional_parameters_units <- myreadLines( fp, n=1 )
				additional_parameters_value <- myreadLines( fp, n=1 )
				}
			}
		}
	if (block_future_upgrade_entries > 0 ) {
		future_upgrade_block_entries <- myreadLines( fp, n=block_future_upgrade_entries )
		}
	num_ordinate_values <- myreadLines( fp, n=1 )
	for (var in 1:corresponding_variables_num) {
			minimum_ordinate_values <- myreadLines( fp, n=1 )
			maximum_ordinate_values <- myreadLines( fp, n=1 )
		}
	Y <- as.numeric(myreadLines( fp, n=num_ordinate_values ))
	# =================================================================
	## Rxps XPSCoreLine creation of data
	tmpmat <- matrix(data=as.numeric(Y),
				ncol=as.numeric(corresponding_variables_num),
				byrow=TRUE)
	colnames(tmpmat) <- sapply(corresponding_variables, function(x) unlist(x$label))
	tmp <- apply(tmpmat, 2, list)
	tmp <- lapply(tmp, function(x) unlist(x))
	data <- list(x=seq(from=as.numeric(abscissa_start),
						by=as.numeric(abscissa_increment),
						length.out=as.numeric(num_ordinate_values)/as.numeric(corresponding_variables_num)))
	data <- c(data, tmp)
	
	## Check binding or Kinetic or ...					
	binding <- ifelse ( attr(regexpr("Binding", abscissa_label), "match.length" ) > 0 , TRUE, FALSE)
	cps <- FALSE
	## Transform Kinetic to Binding
	if ( ! binding ) {
		data$x <- as.numeric(analysis_source_energy) - data$x
		binding <- TRUE
		}
		
	## original units and flags
	units <- c(paste(abscissa_label, " [",abscissa_unit,"] ", sep=""), paste(corresponding_variables[[1]]$label, " [",corresponding_variables[[1]]$units,"]", sep=""))

	## Transform Counts to cps	
	## i parametri simili sono:
	## analysis_source_energy == 1486.6
	## analyser_pass_energy
	## magnification_transfer_lens ?? serve???
	## signal mode == 'pulse counting'
	## signal_collection_time == in secondi, nei commenti puo' essere in ms (eg:Dwell Time(ms): 100 == 0.1)
	## num_scans_block == n. sweeps
	## acq. time == signal_collection_time * num_scans_block * (num_ordinate_values/corresponding_variables_num)
	## acq_time <- as.numeric(signal_collection_time) * as.numeric(num_scans_block) * as.numeric(num_ordinate_values)/as.numeric(corresponding_variables_num)
	
   data[[2]] <- data[[2]]/data[[3]]  #correzione per Trasmissione Analizzatore
	dwelltime <- as.numeric(signal_collection_time)
	data[[2]] <- data[[2]]/dwelltime/as.numeric(num_scans_block)
	cps <- TRUE
	##cat("\n Symbol : ", block_identifier, "\t Time acquisition : ", acq_time, "\t Dwell Time : ", dwelltime, "\t Sweeps : ", num_scans_block)
	
	# change of units & flag
	if ( binding ) { units <- c("Binding Energy [eV]","Intensity [cps]") }
		
	## Symbol adjust
	blkidentif <- gsub(" ", "", block_identifier)
	newsymbol <- unlist(strsplit(blkidentif,"/"))[1]
	## vamas block XPSCoreLine
	## 18/9/2012 Add isScienta Flags[3] == FALSE for vms files
	vamasBlock <- new("XPSCoreLine",
						.Data = data,
						units = units,
						Flags = c(binding, cps, FALSE, TRUE),
						Info  = block_comments_lines,
						Symbol = newsymbol
						)

	return(vamasBlock)
}

read_experiment <- function(fp) {
	format_identifier <- myreadLines( fp, n=1 )
	institution_identifier <- myreadLines( fp, n=1 )
	instrument_model_identifier <- myreadLines( fp, n=1 )
	operator_identifier <- myreadLines( fp, n=1 )
	experiment_identifier <- myreadLines( fp, n=1 )
	num_lines_in_comment <- myreadLines( fp, n=1 )
	comments_lines <- ""
	if (num_lines_in_comment >0) comments_lines <- myreadLines( fp, n=num_lines_in_comment )
	experiment_mode <- myreadLines( fp, n=1 )
	experiment_mode <- match.arg(experiment_mode, c("MAP","MAPDP","MAPSV","MAPSVDP","NORM","SDP","SDPSV","SEM" ))
	scan_mode <- myreadLines( fp, n=1 )
	if ( experiment_mode %in% c("MAP","MAPDP","NORM","SDP") )
		num_spectral_region <- myreadLines( fp, n=1 )

	if ( experiment_mode %in% c("MAP","MAPDP") ) {
		num_analysis_position <- myreadLines( fp, n=1 )
		x_coord <- myreadLines( fp, n=1 )
		y_coord <- myreadLines( fp, n=1 )
		}
	exp_variables <- myreadLines( fp, n=1 )
	if ( exp_variables > 0 ) {
		for (var in 1:exp_variables) {
			exp_variables_labels <- myreadLines( fp, n=1 )
			exp_variables_units <- myreadLines( fp, n=1 )
			}
		}
	#
	entry_in_inclusion_list <- myreadLines( fp, n=1 )
	d <- ifelse(entry_in_inclusion_list > 0, 0, 1)
	inclusion_list <- rep.int(d, times=40)
	include <- rep.int(1, times=40)
	d <- ifelse(d,0,1)
	if (entry_in_inclusion_list < 0) entry_in_inclusion_list <- entry_in_inclusion_list*(-1)
	if (entry_in_inclusion_list > 0) {
		for (var in 1:entry_in_inclusion_list) {
			idx <- myreadLines( fp, n=1 )
			inclusion_list[idx] <- d
			}
		}
	#
	num_items_in_block <- myreadLines( fp, n=1 )
	if (num_items_in_block > 0) {
		prefix_number_items <- myreadLines( fp, n=num_items_in_block )
		}
	exp_future_upgrade_entries <- myreadLines( fp, n=1 )
	block_future_upgrade_entries <- myreadLines( fp, n=1 )
	if (exp_future_upgrade_entries > 0 ) {
		future_upgrade_entries <- myreadLines( fp, n=block_future_upgrade_entries )
		}
	number_of_blocks <- myreadLines( fp, n=1 )
	#cat("include = ",include,"\n")
	#number_of_blocks <- 2
	
	vamasSample <- new("XPSSample", Project=institution_identifier, Sample=experiment_identifier, Comments=comments_lines, User=operator_identifier, Filename="")
	## loop sui blocchi
	for ( block in 1:number_of_blocks) {
		if (block == 2) {
			include <- inclusion_list
		}
		vamasSample[[block]] <- read_block(fp, include, experiment_mode, exp_variables, scan_mode, block_future_upgrade_entries)
	}
	readLines( fp, n=1 ) # experiment terminator
	return(vamasSample)
}

## ================================================
# Start of main code
## ================================================

		if (is.null(file)) file <- file.choose()
		fp <- file(file, open="r")
		on.exit(close(fp))
		object <- read_experiment(fp)
		names(object) <- sapply(object, function(x) x@Symbol[1])

		return(object)
} ## end read.vamas