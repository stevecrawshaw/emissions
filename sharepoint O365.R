pacman::p_load(Microsoft365R)


od <- get_business_onedrive()

od$list_files(path = 'Documents')

hjt <- od$get_item('Documents/historic_journey_time_-_after_1_Jan_16.csv')


hjt_tbl <- od$load_dataframe('Documents/historic_journey_time_-_after_1_Jan_16.csv')


list_sharepoint_sites()
# need admin permissions