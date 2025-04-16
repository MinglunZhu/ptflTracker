// Function to check orientation and set showLegend value
function updateSettingsByOrientation() {
    // isLandscape will be true if width > height
    Shiny.setInputValue('isLandscape', window.matchMedia('(orientation: landscape)').matches, { priority: 'event' });
}

$(document).ready(function() {
    const   HDR = $('.main-header'), // Target the header
            TGL_BTN = $('#header-toggle-btn'); // Target the button
    let isMouseOverHdr = false;

    TGL_BTN.on('click', function() {
        HDR.addClass('active');
    });

    // Keep the header open when interacting with it
    HDR.on('mouseenter', function() {
        isMouseOverHdr = true;
    });

    function checkDeactivateHeader() {
        // Deactivate only if mouse is NOT over header AND no dropdowns are shown
        // Check both Bootstrap dropdowns and Selectize dropdowns
        const ANY_OPEN = $('.selectize-dropdown.multi').is(':visible')  
            || $('.dropdown.open').length > 0 
            || $('.dropdown-menu.show').length > 0;

        // Deactivate only if:
        // 1. Header currently IS active
        // 2. Mouse is NOT over the header area
        // 3. NO dropdown menu is open
        if (HDR.hasClass('active') && !isMouseOverHdr && !ANY_OPEN) HDR.removeClass('active');
    }

    HDR.on('mouseleave', function() {
        isMouseOverHdr = false;

        // Check conditions immediately on mouse leave
        checkDeactivateHeader();
    });

    // Use both standard Bootstrap and accessibility plugin events
    $(document).on('hidden.bs.dropdown hide.bs.dropdown', function () {
        setTimeout(checkDeactivateHeader, 0);
    });

    // Listen for R's request to check orientation
    Shiny.addCustomMessageHandler("check-orientation", function(message) {
        updateSettingsByOrientation();

        // Initialize after dataInit is ready
        // change event means either it matches landscape or not
        window.matchMedia('(orientation: landscape)').addEventListener("change", updateSettingsByOrientation);
        // screen orientatin doesn't account for browser resize
        //screen.orientation.addEventListener("change", updateLegendByOrientation);
        // Also handle resize for browsers/devices that don't support orientationchange
        //window.addEventListener('resize', updateLegendByOrientation);
    });
});
